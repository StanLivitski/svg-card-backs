#!/usr/bin/env python
# vim:fileencoding=UTF-8 
################################
# Copyright © 2017, 2019 Stan Livitski
# 
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License  as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
"""
Command-line tool for preparing upstream SVG images of card backs
for use in the ``cards.webapp`` project, as follows:

 - add ``xmlns="http://www.w3.org/2000/svg"`` to the ``/svg`` element
   for proper rendering in browsers;
 - set width of the images using the root element's `width` parameter
   to achieve an aspect ratio of the image of *1:sqrt(2)*;
 - set the image's ``preserveAspectRatio`` attribute to "none" to
   enable scaling it to the target aspect ratio;
 - change comments to add the new author, update copyrights, and
   remove misleading information;
 - optimize repetitive styles by moving them from the DTD to a ``<style>``
   element, since XSLT 1.0 does not allow for custom DTD within XML output
   (this optimization may be turned off using a command-line switch);
 - create images of stacked card backs and bundle them with original
   image for subsequent processing.

Dependenices
------------

+-----------------------------------------------------------+---------------+
|  Name / Download URL                                      | Version       |
+===========================================================+===============+
| | Python                                                  | 3.4 or newer  |
| | https://www.python.org/downloads/ or an OS distribution |               |
+-----------------------------------------------------------+---------------+
| | ``click`` package                                       | 6.3 or newer  |
| | https://pypi.python.org/pypi/click or                   |               |
| | http://click.pocoo.org/                                 |               |
+-----------------------------------------------------------+---------------+
| | ``svgpathtools`` package                                | 1.3.3, newer  |
| | https://pypi.org/project/svgpathtools/ or               | versions may  |
| | https://github.com/mathandy/svgpathtools                | or may not    |
| |                                                         | work          |
+-----------------------------------------------------------+---------------+
| | ``svgwrite`` package                                    | dependency of |
| | https://pypi.org/project/svgwrite/ or                   | `svgpathtools`|
| | http://github.com/mozman/svgwrite.git                   | not used here |
+-----------------------------------------------------------+---------------+
| | ``numpy`` package                                       | 1.14 or newer |
| | https://pypi.org/project/numpy/ or                      |               |
| | https://www.numpy.org/                                  |               |
+-----------------------------------------------------------+---------------+
| | ``pyparsing`` package                                   | dependency of |
| | https://pypi.org/project/pyparsing/ or                  | ``svgwrite``  |
| | https://github.com/pyparsing/pyparsing/                 | not used here |
+-----------------------------------------------------------+---------------+
| | ``py_dom_xpath_six`` package                            | 0.2.3         |
| | https://pypi.org/project/py-dom-xpath-six/              |               |
| | https://github.com/jackjansen/py-dom-xpath-six/         |               |
+-----------------------------------------------------------+---------------+
| | ``future`` package                                      | any compatible|
| | https://pypi.org/project/future/ or                     | with ``py_dom |
| | https://python-future.org/                              | _xpath_six``  |
+-----------------------------------------------------------+---------------+

**NOTE:** ``svgpathtools/path.py`` must be patched by changing line 2196  from::

 < pts = [seg1.point(_t1) for _T1, _seg1, _t1 in list(zip(*intersecti

to::

 > pts = [_seg1.point(_t1) for _T1, _seg1, _t1 in list(zip(*intersection_list))[0]]

(see `svgpathtools bug report #94<https://github.com/mathandy/svgpathtools/issues/94>`_ for details)

See also
--------

make_stack : Command-line entry point of the tool. See PyDoc comment
    for specification.
"""
import sys

if 'version_info' not in dir(sys) or sys.version_info[0] < 3 or (
     sys.version_info[0] == 3 and sys.version_info[1] < 5):
    sys.stderr.write('This program requires Python version 3.5 or newer.\n')
    sys.exit(1)

import click
from collections import abc
from datetime import date
import io
import logging
import math
from numbers import Number
from os import path
import os
import re
from svgpathtools import svg_to_paths as svgpath
from svgpathtools.parser import parse_path
import svgpathtools.path
import sys
from xml.dom import Node, IndexSizeErr
from xml.dom.minidom import parse as parseXML
from xml.dom.minidom import parseString as parseXMLString
import xpath
from xpath.exceptions import *

class Path(svgpathtools.path.Path, abc.Hashable):

    def __new__(class_, *args, **kwargs):
        self = (parse_path(kwargs['d']) if 'd' in kwargs
                else svgpathtools.path.Path(*args, **kwargs))
        self.__class__ = class_
        return self

    def __init__(self, *args, **kwargs):
        self._hash = None

    def __setitem__(self, index, value):
        self._hash = None
        super().__setitem__(index, value)

    def __delitem__(self, index):
        self._hash = None
        super().__delitem__(index)

    def __hash__(self):
        if self._hash is None:
            self._hash = hash(self.d()) if self else 0
        return self._hash

    def insert(self, index, value):
        self._hash = None
        super().insert(index, value)

    def cropped(self, T0, T1):
        return Path(*super().cropped(T0, T1))

    def split(self, t):
        """
        Return two segments, whose union is this Path and which join
        at self.point(t).
        """

        return (self, Path()) if t >= 1 else \
               (Path(), self) if t <= 0 else \
               (self.cropped(0, t), self.cropped(t, 1))

    def continuous_subpaths(self):
        """
        Return a list of continuous subpaths of the same class as
        this object.
        """

        subpaths = []
        i = start = 0
        for seg in self:
            i += 1
            if seg.end != self[i if i < len(self) else 0].start:
                subpaths.append(Path(*self[start:i]))
                start = i
        if start < len(self):
            subpaths.append(Path(*self[start:]))
        return subpaths

    def d(self, useSandT=False, use_closed_attrib=False):
        hash_ = last_seg = None
        if self.iscontinuous() and self.isclosed()\
           and isinstance(self[-1], svgpathtools.path.Line):
            hash_ = self._hash
            last_seg = self.pop()
        try:
            d = super().d(useSandT, use_closed_attrib)
            return d if last_seg is None else d + ' Z'
        finally:
            if last_seg is not None:
                self.append(last_seg)
                self._hash = hash_

def traverseDOM(subtree, handler, args = [], kwargs = {}):
    result = handler(subtree, *args, **kwargs)
    if result is not None:
        return result
    elif subtree.parentNode is not None or subtree.nodeType == Node.DOCUMENT_NODE:
        # delve into descendants only if the node still belongs to the tree
        # iteration over subtree.childNodes will fail if handler deletes a node
        node = subtree.firstChild
        while node is not None:
            next_ = node.nextSibling
            result = traverseDOM(node, handler, args, kwargs)
            if result is not None:
                return result
            node = next_
    return None

def binsearch(seq, tgt, reversed = False, key = lambda x: x):
    """binsearch(sequence, object, callable, boolean) -> int
    
    Search a sorted sequence, `seq`, for a target value, `tgt`,
    and return its index, or ``-1`` if nothing is found. Items
    in the sequence, or the values returned by `key` function,
    must support comparison operators. If the `key` function
    is provided, `tgt` is compared with its results.
    """

    top = 0
    bottom = len(seq)
    at = -1
    while top < bottom:
        at = (top + bottom) >> 1
        key_ = key(seq[at])
        if key_ == tgt: break
        elif key_ < tgt if reversed else key_ > tgt:
            bottom = at
        else:
            top = at + 1
        at = -1
    return at 

class ErrorSuppressor(abc.Callable):

    def __init__(self, code, errtypes, log, level = logging.WARNING, retval = None):
        self.code = code
        self.type = errtypes
        self.log = log
        self.loglevel = level
        self.retval = retval

    def __call__(self, *args, **kwargs):
        try:
            return self.code(*args, **kwargs)
        except:
            if issubclass(sys.exc_info()[0], self.type):
                if self.log is not None:
                    self.log.log(self.loglevel, '%s in %s',
                                 sys.exc_info()[0].__name__,
                                 self.code, exc_info = True)
                return self.retval
            raise

class CommentUpdater(abc.Callable):

    def __init__(self, log):
        self.log = log

    class Replacement(abc.Callable):
        ''' Replace a comment's text, remove a comment, or
            add comments before and/or after this one.
        '''
        def __init__(self, text = None, before = None, after = None, remove = False):
            self.text = text
            self.before = before
            self.after = after
            self.remove = remove

        def __call__(self, node): # called by CommentUpdater
            doc = node.ownerDocument
            parent = node.parentNode
            before = (self.before,) if isinstance(self.before, str) else self.before
            if before is not None:
                for comment in before:
                    parent.insertBefore(doc.createComment(comment), node)
            after = (self.after,) if isinstance(self.after, str) else self.after
            if after is not None:
               before = node.nextSibling
               for comment in after:
                   parent.insertBefore(doc.createComment(comment), before)
            if self.remove:
                parent.removeChild(node).unlink()
                return None
            return self.text

    CURRENT_YEAR = date.today().year
    ADD_COPYRIGHT_YEAR = '' if 2019 == CURRENT_YEAR else ',%d' % CURRENT_YEAR

    HANDLER_CONTACT = (lambda comment: 0 <= comment.find('bellot@'),
                         Replacement(remove = True))
    NOOP = Replacement()

    HANDLERS = {
        'generator:': (lambda comment: 0 <= comment.find('Generator: '),
             Replacement(remove = True)
            ),
        'set': (lambda comment: 0 <= comment.find('This set of SVG files is'),
             Replacement("    This set of SVG files contains images of playing cards'"
                         " backs		")
            ),
        'backs': (lambda comment: 0 <= comment.find('and backs of cards completely '),
             Replacement("    adapted from version 1.1 of the upstream project"
                         "			",
                         after = "    <https://sourceforge.net/projects/svg-cards/>"
                         "				")
            ),
        'copyright': (lambda comment: 0 <= comment.find('Copyright '),
             Replacement(after = '    Copyright © 2017,2019' + ADD_COPYRIGHT_YEAR
                         + ' Stan Livitski				'
                         + ('' if ADD_COPYRIGHT_YEAR else '\t'))
            ),
        'modify': (lambda comment: 0 <= comment.find('GNU Lesser General Public'),
             NOOP
            ),
        'license': (lambda comment: 0 <= comment.find('Free Software'),
             NOOP
            ),
        'version':
            (lambda comment: 0 <= comment.find('version 2 of the License'),
             lambda node: node.data.replace('version 2 ', 'version 3 ')),
        'this': (lambda comment: 0 <= comment.find('This library is '),
             NOOP
            ),
        'warranty': (lambda comment: 0 <= comment.find('WITHOUT ANY WARRANTY'),
             NOOP
            ),
        'MERCHANTABILITY'.lower(): (lambda comment: 0 <= comment.find(
            'or FITNESS FOR A PARTICULAR PURPOSE'),
             NOOP
            ),
        'lesser': (lambda comment: 0 <= comment.find(
            'Lesser General Public'),
             NOOP
            ),
        'foundation,': (lambda comment: 0 <= comment.find(
            '59 Temple Place, Suite 330, Boston, MA'),
             NOOP
            ),
        'contact': HANDLER_CONTACT,
        'or': HANDLER_CONTACT,
    }

    def __call__(self, node): # CommentUpdater
        if node.nodeType != Node.COMMENT_NODE:
            return
        log = self.log
        handler = None
        comment = node.data
        while comment:
            comment = comment.split(maxsplit=1)
            if 1 < len(comment):
                handler, comment = comment
            elif 1 == len(comment):
                handler = comment[0]
                comment = None
            else:
                handler = None
                break
            handler = self.HANDLERS.get(handler.lower())
            if handler is None:
                continue
            if handler[0](node.data):
                handler = handler[1]
                break
            else:
                handler = None
        if handler is not None:
            result = handler(node)
            if result is not None:
                node.data = result
        elif log is not None and log.isEnabledFor(logging.INFO) and node.data.strip():
            log.info("Please review SVG comment:\n%s",
                     node.data)

class PathExtractor(abc.Callable):

    def __init__(self, log):
        self.log = log
        self.reset()

    def reset(self):
        self.paths = {}

    def poll(self, reset=True):
        paths = self.paths
        if self.log is not None and self.log.isEnabledFor(logging.DEBUG):
            self.log.debug("Found %d path(s)", len(paths))
        self.reset()
        return paths

    COMMENT_DUMMY_NODE = "!svg-card-backs:stack:see='preceding-sibling'"
        
    def _addpath(self, path, node):
        existing = self.paths.get(path)
        if existing is None:
            self.paths[path] = (node,)
        elif isinstance(existing, abc.MutableSequence):
            if not any(n.isSameNode(node) for n in existing):
                existing.append(node)
        elif isinstance(existing, abc.Sequence):
            if not any(n.isSameNode(node) for n in existing):
                existing = self.paths[path] = list(existing)
                existing.append(node)
        elif not existing.isSameNode(node):
            self.paths[path] = [existing, node]

    def __call__(self, node):
        if node.nodeType != Node.ELEMENT_NODE:
            return
        log = self.log
        method = '_read_' + node.tagName
        d = None
        if node.hasAttribute('transform'):
            raise NotImplemented(
                'Found element <%s> with unsupported attribute "transform": %s'
                % (node.tagName, node.toxml[:80]))
        elif hasattr(self, method):
            d = getattr(self, method)(node)
        else:
            if log is not None and node.tagName not in {'svg', 'g', 'defs', 'style'}:
                log.warning('Skipping unsupported element: %s',
                            node.toxml()[:160])
            return
        if d is not None:
            path = Path(d = d)
            if path.iscontinuous():
                self._addpath(path, node)
            else:
                parent = node.parentNode
                nextNode = node.nextSibling
                doc = node.ownerDocument
                i = 0
                for frag in path.continuous_subpaths():
                    if log is not None and log.isEnabledFor(1):
                        log.log(1, "Processing path fragment #%d: %s",
                                  i, frag)
                    if node is None:
                        node = doc.createComment(self.COMMENT_DUMMY_NODE)
                        parent.insertBefore(node, nextNode)
                    self._addpath(frag, node)
                    node = None
                    i += 1
        elif log is not None:
            log.warning('Element %s contained no path info.', node.toxml())

    @staticmethod
    def _read_path(node):
        d = node.getAttribute('d')
        return d if d else None

    @staticmethod
    def _read_polyline(node):
        p = node.getAttribute('points')
        return svgpath.polyline2pathd(p) if p else None

    @staticmethod
    def _read_polygon(node):
        p = node.getAttribute('points')
        return svgpath.polygon2pathd(p) if p else None

    def _attrs2path(self, node, handler):
        try:
            attrs = dict( i for i in node.attributes.items() )
            return handler(attrs)
        except:
            if self.log is not None:
                self.log.warning(
                    'Error processing tag: %s',
                    node.toxml(),
                    exc_info = sys.exc_info()[1],
                )
            return None 

    def _read_rect(self, node):
        return self._attrs2path(node, svgpath.rect2pathd)
    
    def _read_ellipse(self, node):
        return self._attrs2path(node, svgpath.ellipse2pathd)

    def _read_circle(self, node):
        return self._attrs2path(node, svgpath.ellipse2pathd)

    LINE_ATTRS = ('x1','y1','x2','y2')

    def _read_line(self, node):
        coords = [ node.getAttribute(a) for a in self.LINE_ATTRS ]
        for i in range(len(coords)):
            try:
                c = coords[i] = float(coords[i])
                if not math.isfinite(c):
                    raise ValueError()
            except:
                if self.log is not None:
                    self.log.warning(
                        'Required atribute %s is invalid or missing in: %s',
                        self.LINE_ATTRS[i],
                        node.toxml()
                    )
                return None
        else:
            return 'M{} {} L{} {}'.format(*coords)

class StyleCollector(abc.Callable):

    xpath_style_container = 'defs/style'
    styleClassFormat = 'st%d'

    def __init__(self, log):
        self.log = log
        self.styles = {}
        self.counter = 0

    def __call__(self, node):
        if node.nodeType != Node.ELEMENT_NODE or \
           not self.xpath_style_container or \
           not node.hasAttribute('style'):
            return
        def addClass(element, class_):
            classes = set(element.getAttribute('class').split())
            classes.add(class_)
            element.setAttribute('class', ' '.join(classes))
        style = node.getAttribute('style').strip()
        group = self.styles.get(style)
        if isinstance(group, str):
            node.removeAttribute('style')
            addClass(node, group)
        elif isinstance(group, Node):
            self.counter += 1
            class_ = self.styleClassFormat % (self.counter)
            for e in (group, node):
                e.removeAttribute('style')
                addClass(e, class_)
            self.styles[style] = class_
        else:
            assert group is None
            self.styles[style] = node

    def saveStyles(self, doc, xpath_):
        if self.counter == 0:
            return
        classes = dict( (v, k) for k, v in self.styles.items()
                        if isinstance(v, str) )
        container = xpath_.resolveTargetPath(
            self.xpath_style_container, doc.documentElement)
        anchor = container.firstChild
        for i in range(self.counter):
            class_ = self.styleClassFormat % (i + 1)
            if not class_ in classes:
                continue
            text = doc.createTextNode(
                '.%s { %s }' % (class_, classes[class_])
            )
            container.insertBefore(text, anchor)

class XPathToolkit:

    def __init__(self, log):
        self.log = log

    AXES = {
        'preceding': lambda node: (node.parentNode, node),
        'preceding-sibling': lambda node: (node.parentNode, node),
        'following': lambda node: (node.parentNode, node.nextSibling),
        'following-sibling': lambda node: (node.parentNode, node.nextSibling),
        'child': lambda node: (node, node.childNodes),
    }

    SUPPORTED_AXES = None

    @classmethod
    def _axis(cls, funckey):
        if cls.SUPPORTED_AXES is None:
            supported = ''
            for axis in tuple(cls.AXES):
                supported += (", '%s'" if supported else "'%s'") % axis
                handler = cls.AXES.pop(axis)
                cls.AXES[xpath.expr.axes[axis]] = handler
            cls.SUPPORTED_AXES = supported
        return cls.AXES.get(funckey)

    @staticmethod
    def _splitPath(path):
        parsed = xpath.XPath.get(path).expr
        if isinstance(parsed, xpath.expr.AbsolutePathExpr):
            isAbsolute = True
            parsed = parsed.path
        else:
            isAbsolute = False
        if not isinstance(parsed, xpath.expr.PathExpr):
            raise XPathNotImplementedError(
                'Unsupported XPath expression "%s" of %s'
                ' found when trying to insert an XML node'
                % (path, type(parsed))
            )
        step = parsed.steps[-1]
        base = xpath.expr.PathExpr(parsed.steps[:-1])
        if isAbsolute:
            base = xpath.expr.AbsolutePathExpr(base)
        base = str(base)
        if not base:
            base = None
        return base, step

    def resolveTargetPath(self, path, context_node):
        try:
            return self._resolveTargetPath(path, context_node)
        except XPathNotImplementedError as e:
            raise e
        except XPathError as e:
            raise type(e)(
                'Error processing XPath expression "%s" in the context of %s'
                ' where a node set result is expected' % (path, 
                '<%s>' % context_node.nodeName if context_node.nodeType == Node.ELEMENT_NODE
                else '@' + context_node.nodeName if context_node.nodeType == Node.ATTRIBUTE_NODE
                else context_node.nodeName) +
                ': %s' % e.args[0] if e.args else '.'
            ) from e

    def _resolveTargetPath(self, path, context_node):
        node = xpath.findnode(path, context_node)
        if node is None:
            log = self.log
            self.log = None # NOT thread-safe
            if log is not None and log.isEnabledFor(logging.INFO):
                log.info('Document contains no node(s) at path "%s"'
                         ' from <%s ...>, will try to create them. Caution:'
                         ' this involves non-API library calls.',
                         path, context_node.nodeName)
            base, step = self._splitPath(path)
            if base is not None:
                context_node = self._resolveTargetPath(base, context_node)
            position = 0
            if isinstance(step, xpath.expr.PredicateList):
                if 1 < len(step.predicates):
                    raise XPathNotImplementedError(
                        'Unsupported XPath predicate in "%s"'
                        ' found when trying to insert an XML node.'
                        ' Only single-element predicates are recognized.'
                        % step)
                predicate = step.predicates[0]
                step = step.expr
                if isinstance(predicate, xpath.expr.LiteralExpr):
                    if not isinstance(predicate.literal, Number) \
                        or 1 > predicate.literal:
                        raise XPathNotImplementedError(
                            'Unsupported XPath predicate value [%s]'
                            ' found when trying to insert an XML node.'
                            ' Only positive integers and last() are allowed here.'
                            % predicate)
                    try:
                        position = int(predicate.literal) - 1
                    except:
                        raise IndexSizeErr(
                            'Invalid XPath index [%s]'
                            ' found when trying to insert an XML node.'
                            % predicate) from sys.exc_info()[1]
                elif isinstance(predicate, xpath.expr.Function):
                    if 'last' != predicate.name:
                        raise XPathNotImplementedError(
                            'Unsupported XPath predicate function "%s"'
                            ' found when trying to insert an XML node.'
                            ' Only literal numbers and last() are allowed here.'
                            % predicate)
                    position = -1
                else:
                    raise XPathNotImplementedError(
                        'Unsupported XPath predicate expression [%s]'
                        ' found when trying to insert an XML node.'
                        ' Only literal numbers and last() are allowed here.'
                        % predicate)
            if not isinstance(step, xpath.expr.AxisStep) or \
               not isinstance(step.test, xpath.expr.NameTest):
                raise XPathNotImplementedError(
                    'Unsupported test "%s" found in XPath expression "%s"'
                    ' when trying to insert an XML node.'
                    ' Only non-wildcard `NameTest` is allowed here.'
                    % (step, path)
                )
            axis = self._axis(step.axis)
            if axis is None:
                raise XPathNotImplementedError(
                    'Unsupported axis found in XPath expression "%s"'
                    ' when trying to insert an XML node.'
                    ' Only %s axes are allowed here.'
                    % (step, self.SUPPORTED_AXES)
                )
            name = step.test.localName
            if step.test.prefix is not None:
                name = step.test.prefix + ':' + name
            parent, anchor = axis(context_node)
            if isinstance(anchor, abc.Sequence):
                try:
                    if 0 < position:
                        anchor = [ e for e in anchor \
                                   if Node.ELEMENT_NODE == e.nodeType and \
                                   name == e.nodeName ]
                        anchor.append(None)
                        anchor = anchor[position]
                    elif 0 == position:
                        anchor = anchor[0] if anchor else None
                    else:
                        anchor = None
                except:
                    raise IndexSizeErr(
                        'Node index %d out of range for XPath expression'
                        ' "%s".'
                        % (position + 1, path)) from sys.exc_info()[1]                
            elif 0 != position:
                raise XPathNotImplementedError(
                    'Unsupported position found in XPath expression "%s"'
                    ' when trying to insert an XML node.'
                    ' Only position 1 (default) is allowed for siblings.'
                    % step)
            if '*' in (step.test.prefix, step.test.localName):
                 raise XPathNotImplementedError(
                    'Wildcard test found in XPath expression "%s"'
                    ' when trying to insert an XML node.'
                    ' Only non-wildcard `NameTest` is allowed here.'
                    % path)
            node = parent.insertBefore(
                parent.ownerDocument.createElement(name),
                anchor)
            self.log = log
        return node

class TextCleaner(abc.Callable):
    
    TAGS_EPHEMERAL =  {'g', 'defs', 'style', 'svg', 'symbol'}

    def __init__(self, log, removeEmptyTags = TAGS_EPHEMERAL):
        self.log = log
        self.text = ''
        self.lastElement = None
        self.removeEmptyTags = removeEmptyTags

    PATTERN_LSPACE = re.compile(r'^\s*')
    PATTERN_RSPACE = re.compile(r'\s*$')
    PATTERN_EOL = re.compile(r'[\r\n]')

    def __call__(self, node):
        if Node.ELEMENT_NODE == node.nodeType:
            self.lastElement = node
        if node.nodeType != Node.TEXT_NODE:
            return
        if node.nextSibling is not None and node.nextSibling.nodeType == Node.TEXT_NODE:
            self.text += node.data
            node.parentNode.removeChild(node).unlink()
        else:
            text = self.text + node.data
            wsinfo = self.PATTERN_LSPACE.match(text)
            if wsinfo is None or 0 == wsinfo.end():
                pass
            elif wsinfo.end() == len(text):
                text = ''
            else:
                text = ('\n' if self.PATTERN_EOL.search(wsinfo[0]) else ' ') + text[wsinfo.end():]
            wsinfo = self.PATTERN_RSPACE.search(text)
            if wsinfo is not None and len(text) > wsinfo.start():
                text = text[:wsinfo.start()] + ('\n' if self.PATTERN_EOL.search(wsinfo[0]) else ' ')
            if text:
                node.data = text
            elif node.nextSibling is None and self.lastElement is not None and \
               node.parentNode.isSameNode(self.lastElement):
                emptyNode = node.parentNode
                emptyNode.removeChild(node).unlink()
                if self.removeEmptyTags and emptyNode.localName in self.removeEmptyTags and \
                 (emptyNode.namespaceURI is None or
                  emptyNode.namespaceURI == Converter.NAMESPACE_SVG) and \
                 emptyNode.parentNode is not None:
                    emptyNode.parentNode.removeChild(emptyNode).unlink()
            else:
                node.parentNode.removeChild(node).unlink()                
            self.text = ''

class Converter:

    NAMESPACE_SVG = "http://www.w3.org/2000/svg"
    NAMESPACE_XLINK = "http://www.w3.org/1999/xlink"

    PATTERN_NUMBER = re.compile(r'[+-]?(?:[0-9]*\.[0-9]+|[0-9]+)(?:[Ee][0-9]+)?')
    PATTERN_COORDINATE_DELIMITER = re.compile(r'\s*,\s*|\s+')

    def __init__(self, log):
        self.log = log
        self.xpath = XPathToolkit(log)
        self.reset()

    def reset(self, in_=None, out=None):
        self.in_ = in_
        self.out = out
        if hasattr(self, 'doc') and self.doc is not None:
            self.doc.unlink()
        self.doc = None

    def fixNamespaces(self):
        root = self.doc.documentElement
        name = 'xmlns'
        if root.prefix:
            name += ':' + root.prefix
        if not root.hasAttribute(name):
            root.setAttribute(name, self.NAMESPACE_SVG)
        root.setAttribute(name + ':' + 'xlink', self.NAMESPACE_XLINK)

    BOX_ATTRS = ('x', 'y', 'width', 'height') 
    SHAPE_ATTRS = set(PathExtractor.LINE_ATTRS + BOX_ATTRS
                  + ('cx', 'cy', 'r', 'rx', 'ry', 'points'))

    def updatedShapeNode(self, node, pathData): 
        if Node.ELEMENT_NODE == node.nodeType and \
             node.localName == 'path' and (node.namespaceURI is None or \
             node.namespaceURI == self.NAMESPACE_SVG):
            copy = node.cloneNode(True)
        else:
            copy = self.doc.createElementNS(self.NAMESPACE_SVG, 'path')
            dropid = False
            while Node.COMMENT_NODE == node.nodeType and \
                 node.data.startswith(PathExtractor.COMMENT_DUMMY_NODE):
                dropid = True
                if len(node.data) > len(PathExtractor.COMMENT_DUMMY_NODE):
                    try:
                        node = parseXMLString(
                            node.data[len(PathExtractor.COMMENT_DUMMY_NODE):]
                        ).documentElement
                    except:
                        if self.log is not None:
                            self.log.warning('Error parsing XML string '
                                        'in the dummy comment node: %s',
                                         node.data[:160], exc_info = True)
                    break
                elif node.previousSibling is None:
                    break
                node = node.previousSibling
            if Node.ELEMENT_NODE == node.nodeType:
                for child in node.childNodes:
                    copy.appendChild(child.cloneNode(True))
                for i in range(node.attributes.length):
                    attr = node.attributes.item(i)
                    if attr.name not in self.SHAPE_ATTRS and (
                        not dropid or 'id' != attr.name):
                        copy.setAttributeNode(attr.cloneNode(False))
            else:
                if self.log is not None:
                    self.log.warning('Could not find a shape element '
                                'before the dummy comment node in: %s',
                                 node.parentNode.toxml()[:160])
        copy.setAttribute('d', pathData)
        return copy

    @classmethod
    def parseDistance(cls, value):
        match = cls.PATTERN_NUMBER.match(value)
        if not match:
            raise ValueError('"%s" is not a valid SVG length value.' % value)
        return float(match[0]), value[match.end():]

    @staticmethod
    def clipPathFromRect(rect): 
        x0, y0, w, h = rect
        x1, y1 = x0 + w, y0
        x2, y2 = x0 + w, y0 + h
        x3, y3 = x0, y0 + h
        ox = 1 + max(x0, x1, x2, x3)
        oy = 1 + max(y0, y1, y2, y3)
        return Path(d =
            "M%s %s L%s %s L%s %s L%s %s z"
            % (x0, y0, x1, y1, x2, y2, x3, y3)
        ), complex(ox, oy)

    aspectRatio = math.sqrt(2)
    defaultWidth = 100

    def setAspectRatio(self):
        root = self.doc.documentElement
        width = height = unit = None
        if root.hasAttribute('height'):
            height, unit = self.parseDistance(root.getAttribute('height').strip())
            if unit.strip() == '%':
                height = None
        if height is not None:
            width = height / self.aspectRatio
        elif root.hasAttribute('width'):
            width, unit = self.parseDistance(root.getAttribute('width').strip())
            if unit.strip() == '%':
                width = None
        if width is not None:
            if height is None:
                height = width * self.aspectRatio
        else:
            width = self.defaultWidth
            height = width * self.aspectRatio
            unit = 'px'
        root.setAttribute('width', str(width) + unit);
        root.setAttribute('height', str(height) + unit);            
        root.setAttribute('preserveAspectRatio', "none");
        return width, height, unit

    def clippedPaths(self, paths, clipPath, outside):
        log = self.log
        def shouldKeep(point):
            nonlocal clipPath, outside
            return svgpathtools.path.path_encloses_pt(
                     point, outside, clipPath) if point is not None else False
        result = {}
        i = 0
        clipLegend = clipPath.d()
        for p in paths:
            if log is not None and log.isEnabledFor(logging.DEBUG):
                log.debug('Clipping path #%d at boundary "%s" ...', i, clipLegend)
            segments = []
            def addSegments(toAdd):
                nonlocal segments, p
                #closed = p.isclosed()
                for s in toAdd:
                    if segments and segments[-1].end != s.start: #closed and 
                        segments.append(
                            svgpathtools.path.Line(segments[-1].end, s.start))
                    segments.append(s)
            crosses = p.intersect(clipPath)
            crosses.sort(key = lambda cross: cross[0][0], reverse = True)
            clips = sorted(crosses, key = lambda cross: cross[1][0])
            start = 0.0
            poutside = pclosed = None
            closed = p.isclosed() or (
                     p.iscontinuous() and shouldKeep(p.start) and shouldKeep(p.end))
            while crosses:
                cross = crosses.pop()
                if log is not None and log.isEnabledFor(1):
                    log.log(1, "Crossing at %s; %s on clipping path, absolute: %s",
                              *cross, clipPath.point(cross[1][0]))
                seg = p.cropped(start, cross[0][0])
                if log is not None and log.isEnabledFor(1):
                    log.log(1, "Split segment: %s", seg)
                if shouldKeep(seg.point(0.5)):
                    addSegments(seg)
                elif closed:
                    clipIdx = binsearch(clips, cross[1][0],
                                    key = lambda cross: cross[1][0])
                    assert 0 <= clipIdx
                    seg = clipPath.cropped(clips[clipIdx - 1][1][0],
                                           cross[1][0])
                    if seg and poutside is None:
                        bbox = p.bbox()
                        poutside = complex(bbox[0]-1, bbox[2]-1)
                        pclosed = Path(*p,
                            svgpathtools.path.Line(p.end, p.start))
                    if seg and not svgpathtools.path.path_encloses_pt(
                         seg.point(0.5), poutside, pclosed):
                        clipIdx += 1
                        if len(clips) <= clipIdx: clipIdx = 0
                        seg = clipPath.cropped(cross[1][0],
                                               clips[clipIdx][1][0])
                        if seg and not svgpathtools.path.path_encloses_pt(
                             seg.point(0.5), poutside, pclosed):
                            clipIdx -= 1
                            if log is not None:
                                log.warning(
                                    'Both segments around intersection %d'
                                    'are outside the clipped area. Clip '
                                    'points are: %s', clipIdx, clips)
                            seg = []
                            del clips[clipIdx]
                    if seg and not math.isclose(1.0,
                         seg.radialrange(cross[1][1].point(cross[1][2]))[0][1],
                         abs_tol = 1e-9
                         ):
                        assert math.isclose(0.0,
                         seg.radialrange(cross[1][1].point(cross[1][2]))[0][1],
                         abs_tol = 1e-9
                        )
                        seg = seg.reversed()
                    addSegments(seg)
                start = cross[0][0]
            if start < 1.0:
                seg = p.cropped(start, 1.0)
                if shouldKeep(seg.point(0.5)):
                    addSegments(seg)
            if segments:
                if segments[-1].end != segments[0].start and closed:
                    segments.append(
                        svgpathtools.path.Line(segments[-1].end, segments[0].start))
                r = result[p] = Path(*segments)
            i += 1
        return result

    @classmethod
    def parseBox(class_, attrValue):
        attrValue = attrValue.strip()
        box = class_.PATTERN_COORDINATE_DELIMITER.split(attrValue, 3)
        for i in range(len(box)):
            try:
                box[i] = float(box[i])
            except:
                raise ValueError('Invalid box coordinate "%s" at index %d'
                                 % (box[i], i)) from sys.exc_info()[1]
        return tuple(box)

    @staticmethod
    def boxValue(rect):
        return ' '.join(("%.6f" % c for c in rect))

    @staticmethod
    def removeId(node):
        if node.nodeType == Node.ELEMENT_NODE and node.hasAttribute('id'):
            node.removeAttribute('id')

    ignoreClipping = False
    strokeWidthMax = 4.0

    def clip(self, image, clipRect):
        log = self.log
        pathExtractor = PathExtractor(log)
        traverseDOM(image, pathExtractor)
        paths = pathExtractor.poll()
        strokeMargin = ( -self.strokeWidthMax / 2, ) * 2 \
                       + ( self.strokeWidthMax, ) * 2
        clipPath, outside = self.clipPathFromRect(
            ( m + c for m,c in zip(strokeMargin, clipRect)))
        _original_Line_intersect = svgpathtools.path.Line.intersect
        wrapper = ErrorSuppressor(
            _original_Line_intersect, AssertionError, log, retval = [])
        def _patched_Line_intersect(*args, **kwargs):
            nonlocal wrapper
            return wrapper(*args, **kwargs)
        svgpathtools.path.Line.intersect = _patched_Line_intersect
        if log is not None and log.isEnabledFor(logging.DEBUG):
            log.debug("Clipping shapes outside rectangle %s"
                      " with margins %s ...",
                      clipRect, strokeMargin)
        clippedPaths = self.clippedPaths(paths.keys(), clipPath,
                                         outside)
        svgpathtools.path.Line.intersect = _original_Line_intersect
        if log is not None and log.isEnabledFor(logging.DEBUG):
            log.debug("Retained %d path(s)", len(clippedPaths))
        for p, nodes in paths.items():
            if not isinstance(nodes, abc.Sequence):
               nodes = (nodes,)
            for node in nodes:
                if not p in clippedPaths:
                    next_ = node.nextSibling
                    if next_ is not None and Node.COMMENT_NODE == next_.nodeType and \
                         next_.data.startswith(PathExtractor.COMMENT_DUMMY_NODE):
                        if Node.COMMENT_NODE == node.nodeType and \
                         node.data.startswith(PathExtractor.COMMENT_DUMMY_NODE):
                            next_.data = node.data
                        else:
                            next_.data = next_.data + ' ' + node.toxml()
                    node.parentNode.removeChild(node).unlink()
                elif not self.ignoreClipping:
                    p = clippedPaths[p]
                    update = self.updatedShapeNode(node, p.d())
                    node.parentNode.replaceChild(update, node)
                    node.unlink()

    def stripDTD(self):
        doc = self.doc
        if doc.doctype is not None:
            doc.removeChild(doc.doctype).unlink()

    def makeUse(self, id, container, rect, anchor = 'node()[1]'):
        use = container.ownerDocument.createElement('use')
        if len(rect) < len(self.BOX_ATTRS):
            rect = tuple(rect) + ( None, ) * (len(self.BOX_ATTRS) - len(rect)) 
        for attr in zip(self.BOX_ATTRS, rect):
            if attr[1] is not None:
                use.setAttribute(attr[0], str(attr[1]))
        use.setAttribute('xlink:href', '#' + id)
        if isinstance(anchor, str):
            anchor = xpath.findnode(anchor, container)
        container.insertBefore(use, anchor)

    xpath_clip_container = '/svg/defs'
    xpath_shape_trees = '.'

    clipLimitRect = (0, 0, .2, 1)
    clipZoomFactor = (2, 1)
    clipFocus = (0, .5)
    clipIdFormat = 'clip-%(width)sx%(height)s'
    imageIdFormat = 'image-%(width)sx%(height)s'

    def run(self):
        if self.in_ is None or self.out is None:
            raise self.Error(
                '%s object is not ready. Please run reset() with arguments first.'
                % type(self).__name__,
                abort = True
            )
        doc = self.doc = parseXML(self.in_)
        if doc.documentElement.localName != 'svg':
            raise self.Error("Root element <%s> is not an SVG."
                             % doc.documentElement.name)
        def compositeHandler(node, *args, **kwargs):
            nonlocal handlers
            for handler in handlers:
                handler(node, *args, **kwargs)
        commentUpdater = CommentUpdater(self.log)
        styleCollector = StyleCollector(self.log)
        handlers = (commentUpdater, styleCollector)
        traverseDOM(doc, compositeHandler)
        image = doc.documentElement
        gallery = image.cloneNode(False)
        for name in tuple(image.attributes.keys()):
            if name.startswith('xml:'):
                image.removeAttribute(name)
        try:
            subtrees = xpath.find(self.xpath_shape_trees, doc.documentElement)
        except XPathError as e:
            raise type(e)(
                'Error processing XPath expression "%s" in the context of %s' %
                (self.xpath_shape_trees, 
                '<%s>' % doc.documentElement.nodeName) +
                (': %s.' % e.args[0] if e.args else '.') +
                ' Please check the `--shape-trees` command-line argument.'
            ) from e
        if not isinstance(subtrees, abc.Collection) or isinstance(subtrees, str):
            raise self.Error(
                'XPath expression "%s" won`t select element(s)'
                ' of the image file. Please check the `--shape-trees` argument'
                ' on your command line.'
                % self.xpath_shape_trees,
                abort = True
            )
        if 1 == len(subtrees) and Node.ELEMENT_NODE == subtrees[0].nodeType \
           and subtrees[0].localName == 'svg' \
           and (subtrees[0].namespaceURI is None
                or self.NAMESPACE_SVG == subtrees[0].namespaceURI):
            clip = subtrees[0].cloneNode(True)
        elif subtrees:
            clip = doc.createElement('svg')
            for subtree in subtrees:
                clip.appendChild(subtree.cloneNode(True))
        else:
            clip = None
        doc.replaceChild(gallery, image)
        defs = self.xpath.resolveTargetPath(
            self.xpath_clip_container, doc.documentElement
        )
        defs.appendChild(image)
        width, height, unit = self.setAspectRatio()
        viewBox = image.getAttribute('viewBox').strip()
        viewBox = self.parseBox(viewBox) if viewBox else (0, 0, width, height)
        image.setAttribute('viewBox', self.boxValue(viewBox))
        image.setAttribute('width', str(viewBox[2]))
        image.setAttribute('height', str(viewBox[3]))
        traverseDOM(image, TextCleaner(self.log, None))
        scale = (width / viewBox[2], height / viewBox[3])
        clipRect = tuple( b + c * f for b, c, f in zip(
                        viewBox[:2], self.clipLimitRect[:2], viewBox[2:4])) \
                 + tuple( c * f for c, f in zip(
                        viewBox[2:4], self.clipLimitRect[2:4]) )
        focus = tuple( b + c * f for b, c, f in zip(
                        viewBox[:2], self.clipFocus, viewBox[2:4])) 
        handlers = (TextCleaner(self.log), self.removeId)
        clips = []
        while clip is not None:
            defs.insertBefore(clip, defs.firstChild)
            id = self.clipIdFormat % dict(zip(self.BOX_ATTRS,
                  ((c if int(c) != c else int(c)) for c in clipRect)))
            try:
                self.clip(clip, clipRect)
            except:
                if self.log is not None:
                    self.log.error('Error clipping element <clip id="%s">', id,
                                   exc_info = True)
                defs.removeChild(clip)
                break
            traverseDOM(clip, compositeHandler)
            clip.setAttribute('id', id)
            clip.setAttribute('viewBox', self.boxValue(clipRect))
            clip.setAttribute('width', str(clipRect[2]))
            clip.setAttribute('height', str(clipRect[3]))
            clips.insert(0, clip)
            clipRect = tuple( c + (b - c) / f for b, c, f in zip(
                        clipRect[:2], focus, self.clipZoomFactor)) \
                     + tuple( round(c / f) for c, f in zip(
                        clipRect[2:4], self.clipZoomFactor) )
            if max(2, self.strokeWidthMax) > min(clipRect[2:4]):
                clip = None
            else:
                clip = clip.cloneNode(True)
        id = self.imageIdFormat % dict(zip(self.BOX_ATTRS,
                  ((c if int(c) != c else int(c)) for c in viewBox)))
        image.setAttribute('id', id)
        offsetX = 0
        clips.append(image)
        anchor = gallery.firstChild
        for clip in clips:
            self.makeUse(clip.getAttribute('id'),
                         gallery,
                         (offsetX, 0),
                         anchor)
            if clip is not image:
                offsetX += float(clip.getAttribute('width')) 
        gallery.setAttribute('viewBox', self.boxValue(
                                      viewBox[:2] + (viewBox[2] + offsetX, viewBox[3])))
        gallery.setAttribute('width', str((viewBox[2] + offsetX) * scale[0]) + unit)
        styleCollector.saveStyles(doc, self.xpath)
        # calling this early breaks XPath lookups
        self.fixNamespaces()
        # this should follow document traversals due to a possible bug in minidom node removal
        # ( not tested with unlink() )
        self.stripDTD()
        doc.writexml(self.out, '', ' ', '\n')

    class Error(Exception):
        def __init__(self, *args, **kwargs):
            super().__init__(*args)
            self.abort = kwargs.get('abort', False)

        def __repr__(self):
            return '%s.%s %s' % (type(Converter).__name__,
                                type(self).__name__,
                                self)

@click.command()
@click.option('-s', '--shape-trees',
              metavar='XPATH',
              help= 'The document element(s) that contain or constitute'
                    ' paths and other shapes to be included in image clips.'
                    ' The path must point to existing node(s) of the source'
                    ' document. Relative paths are resolved from the'
                    ' root element. Default: '"'%s'"
                      % Converter.xpath_shape_trees
                    )
@click.option('-c', '--clip-container',
              metavar='XPATH',
              help= 'The document element that will store zooming image clips.'
                    ' The path must either point to existing element(s)'
                    ' of the source document or be a `LocationPath` production'
                    ' with at least one step pointing to existing element(s)'
                    ' and all the subsequent steps containing non-wildcard'
                    ' `NameTest` productions. Ambiguous results are'
                    ' resolved by picking the first element. Steps'
                    ' that do not yield a result are resolved by creating a'
                    ' matching node. Not all XPath axes and predicates'
                    ' are recognized when creating node(s). Relative paths'
                    ' are resolved from the root element. Default: "%s"'
                      % Converter.xpath_clip_container)
@click.option('-sc', '--style-container',
              metavar='XPATH',
              help= 'The document element that will store repetitive styles.'
                    ' If empty, repetitive styles will not be optimized,'
                    ' otherwise the value must follow the rules that apply'
                    ' to `--clip-container` option and resolve to a <style>'
                    ' element. Default: "%s"'
                      % StyleCollector.xpath_style_container)
@click.option('-sclass', '--style-class-names',
              metavar='PATTERN',
              help= 'Python format string for interpolation of'
                    ' class names generated to optimize'
                    ' repetitive styles. The pattern must'
                    ' yield valid CSS class names using the'
                    ' numeric identifier of a style. Default: "%s"'
                      % StyleCollector.styleClassFormat)
@click.option('-ar', '--set-aspect-ratio',
              type=float,
              help= 'The ratio of height to width that'
                    ' will be applied to the converted card back'
                    ' images. This must be a finite positive number.'
                    ' Default: %s' % Converter.aspectRatio)
@click.option('-dw', '--default-width',
              type=float,
              help= 'The width that will be applied to images'
                    ' that do not specify their intrinsic size.'
                    ' This must be a finite positive number. Default: %s'
                    % Converter.defaultWidth)
@click.option('-mw', '--margin',
              type=float,
              help= 'The size of invisible margins that will be added'
                    ' to clip edges to hide any clipping artifacts.'
                    ' This must be a finite positive number'
                    ' which should exceed the width of source'
                    ' image`s largest stroke. Default: %s'
                    % Converter.strokeWidthMax)
@click.option('-cid', '--clip-ids',
              metavar='PATTERN',
              help= 'Python format string for interpolation of'
                    ' XML IDs assigned to the bundled image clips. The'
                    ' pattern must yield valid and unique XML IDs using'
                    " named arguments 'x', 'y', 'width', and/or 'height'."
                    ' . Default: "%s"'
                      % Converter.clipIdFormat)
@click.option('-iid', '--image-ids',
              metavar='PATTERN',
              help= 'Python format string for interpolation of XML ID'
                    ' assigned to the processed copy of original image.'
                    ' The pattern must yield valid and unique XML ID using'
                    " named arguments 'x', 'y', 'width', and/or 'height'."
                    ' . Default: "%s"'
                      % Converter.imageIdFormat)
@click.option('-m', '--clip-max', 'max_clip',
              metavar= 'X Y WIDTH HEIGHT',
              type= float,
              nargs= 4,
              help= 'Origin and size of the largest clip to be bundled'
                    ' with processed image. These are expressed as fractions'
                    ' of the original image size, which must be finite numbers'
                    ' between 0.0 and 1.0. Default: %s %s %s %s'
                      % Converter.clipLimitRect)
@click.option('-zs', '--clip-zoom-step',
              metavar= 'HZOOM VZOOM',
              type= float,
              nargs= 2,
              help= 'Divisors to be applied to horizontal and vertical dimensions'
                    ' of progressively zoomed clips. These are expressed as fractions'
                    ' of a previous clip size. They must be finite numbers'
                    ' equal or exceeding 1.0. One of the numbers must be'
                    ' above 1.0. Default: %s %s'
                      % Converter.clipZoomFactor)
@click.option('-zf', '--clip-zoom-focus',
              metavar= 'X Y',
              type= float,
              nargs= 2,
              help= 'Focal point of progressively zoomed down clips'
                    ' expressed as fractions of the original image size,'
                    ' which must be finite numbers'
                    ' between 0.0 and 1.0. Default: %s %s'
                      % Converter.clipFocus)
@click.argument('paths',
                nargs=-1,
                type=click.Path(
                 allow_dash=True
                ))
@click.option('-v', '--verbose', count=True,
              help='Repeat up to 2 times for additional debug info on stderr.')
@click.option('-q', '--quiet', is_flag=True,
              help='Suppresses progress message(s) on the standard error.')
@click.option('-f', '--force', is_flag=True,
              help='Overwrite any existing target files.')
@click.option('--no-clipping', 'noclip', is_flag=True,
              help='Do not clip paths that intersect with zooming areas.')
def make_stack(paths, **kwargs): 
    """
    Create SVG images of stacked card backs by clipping
    an SVG image of a single card back and taylor upstream
    images to other requirements of the `cards.webapp` project.

    Arguments:
    
    \b
     - PATHS locations of one or more files with card back images
       followed by a target file or directory to receive the resulting
       file(s), which defaults to the standard output if there is only
       one argument. The last path may also be a dash (-) to request
       writing multiple files to standard output.
    """

    if kwargs['quiet']:
        logging.basicConfig(level = logging.WARN) 
    else:
        logging.basicConfig(level =
            (logging.INFO, logging.DEBUG, 1)[kwargs['verbose']])
    log = logging.getLogger(__name__)

    target = None
    if not paths:
        log.error(
            'Required PATHS argument missing, run with --help for usage details.'
        )
        sys.exit(1)
    elif 1 == len(paths):
        source = paths
    elif '-' == paths[-1]:
        source = paths[:-1]
    else:
        source = paths[:-1]
        target = paths[-1]

    status = None
    if kwargs['style_container'] is not None:
        StyleCollector.xpath_style_container = kwargs['style_container'].strip()
    if kwargs['style_class_names'] is not None:
        StyleCollector.styleClassFormat = kwargs['style_class_names'].strip()

    maker = Converter(log)
    if kwargs['set_aspect_ratio'] is not None:
        val = kwargs['set_aspect_ratio']
        if not math.isfinite(val) or 0 >= val:
            log.error(
                'Parameter of --set-aspect-ratio must be'
                ' finite and positive, got: %s', val
            )
            sys.exit(2)
        else:
            maker.aspectRatio = val
    maker.ignoreClipping = kwargs['noclip']
    if kwargs['shape_trees'] is not None:
        maker.xpath_shape_trees = kwargs['shape_trees'].strip()
    if kwargs['clip_container'] is not None:
        maker.xpath_clip_container = kwargs['clip_container'].strip()
    if kwargs['default_width'] is not None:
        maker.defaultWidth = kwargs['default_width']
    if kwargs['margin'] is not None:
        maker.strokeWidthMax = kwargs['margin']
    if kwargs['clip_ids'] is not None:
        maker.clipIdFormat = kwargs['clip_ids'].strip()
    if kwargs['image_ids'] is not None:
        maker.imageIdFormat = kwargs['image_ids'].strip()
    if kwargs['max_clip']:
        maker.clipLimitRect = kwargs['max_clip']
        for val in maker.clipLimitRect:
            if not (math.isfinite(val) and 0 <= val <= 1):
                log.error(
                    'Parameters of --max-clip must contain'
                    ' numbers between 0.0 and 1.0, got: %s',
                    val
                )
                sys.exit(2)
    if kwargs['clip_zoom_step']:
        maker.clipZoomFactor = kwargs['clip_zoom_step']
        for val in maker.clipZoomFactor:
            if not math.isfinite(val) or val < 1:
                log.error(
                    'Parameters of --clip-zoom-step must contain'
                    ' numbers equal or above 1.0, got: %s', val
                )
                sys.exit(2)
        if not any(val > 1 for val in maker.clipZoomFactor):
            log.error(
                'At least one parameters of --clip-zoom-step'
                ' must exceed 1.0, got: %s %s', *maker.clipZoomFactor
            )
            sys.exit(2)
    if kwargs['clip_zoom_focus']:
        maker.clipFocus = kwargs['clip_zoom_focus']
        for val in maker.clipFocus:
            if not (math.isfinite(val) and 0 <= val <= 1):
                log.error(
                    'Parameters of --clip-zoom-focus must contain'
                    ' numbers between 0.0 and 1.0, got: %s', val
                )
                sys.exit(2)

    for sourcePath in source:
        if '-' == sourcePath:
            log.error(
                'Skipping "-" as it is only allowed as the last argument.'
            )
            if status is None:
                status = 2
            continue

        targetPath = None
        if target is None:
            pass
        elif path.isdir(target):
            fname = path.basename(sourcePath)
            targetPath = path.join(target, fname)
        else:
            targetPath = target

        out = None
        if targetPath is None:
            pass
        elif path.exists(targetPath) and not kwargs['force']:
            log.error(
                ('Skipping "%s", because file "%s" already exists.'
                 + ' Delete it first, or use option -f to overwrite.'
                ) % (sourcePath, targetPath)
            )
            if status is None:
                status = 2
            continue
        else:
            out = open(targetPath, 'w', 1, 'utf_8')

        in_ = None
        try:
            if log.isEnabledFor(logging.INFO):
                log.info('Converting image "%s" to %s ...', sourcePath,
                      'stdout' if targetPath is None else '"%s"' % targetPath)
            in_ = open(sourcePath, encoding='utf_8')
            maker.reset(in_, sys.stdout if out is None else out)
            maker.run()
        except:
            error = sys.exc_info()
            if isinstance(error[1], Converter.Error):
                if 1 < kwargs['verbose']:
                    log.error('Could not write image "%s"',
                              sourcePath, exc_info = error)
                else:
                    log.error('Could not write image "%s": %s',
                              sourcePath, error[1])
                if status is None:
                    status = 4
                if error[1].abort:
                    break
            else:
                if 0 < kwargs['verbose']:
                    log.error('', exc_info = error)
                else:
                    log.error('%s: %s', error[0].__name__, error[1])
                if status is None:
                    status = 3
                if issubclass(error[0], XPathError) and not \
                   issubclass(error[0], XPathNotImplementedError):
                    break
        finally:
            if out is not None:
                out.close()
            if in_ is not None:
                in_.close()
    maker.reset()

    if target is None:
        log.info(
            'Wrote SVG output to standard out, run with --help for usage details.'
        )
    if status is not None:
        sys.exit(status)

if __name__ == '__main__':
    make_stack()
