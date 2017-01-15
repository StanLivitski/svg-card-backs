<?xml version="1.0" encoding="utf-8" ?>
<!--
    Copyright © 2017 Stan Livitski

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License  as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -->

<!-- 
*****************************************************************************
XSL transformation that changes an upstream back image of a card as follows:

 - adds xmlns="http://www.w3.org/2000/svg" to the /svg element for proper
   rendering in browsers;
 - sets width of the image using the `width` parameter that defaults
   to '171.5900947pt', which results in aspect ratio of the image of
   1:sqrt(2);
 - sets the image's preserveAspectRatio attribute to "none" to enable
   scaling it to the target aspect ratio;
 - changes comments to add the new author, update copyrights, and
   remove misleading information;
 - optimizes repetitive styles by moving them from the DTD to a <style>
   element, since XSLT 1.0 does not allow for custom DTD within XML output.

Note that the above optimization may not work with some older SVG rendering
software that does not support CSS. If that is a problem for your project,
use the un-optimized version of this transformation from file "prepare-back-no-style.xslt".
*****************************************************************************
-->
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
 xmlns:svg="http://www.w3.org/2000/svg" exclude-result-prefixes="#default"
>

 <xsl:param name="width" select="'171.5900947pt'" />

 <xsl:output method="xml" indent="yes" cdata-section-elements="svg:style" />

 <xsl:key name="style" match="/svg//*[@style]" use="@style" />

 <xsl:template match="/svg">

  <!-- extract unique styles -->
  <xsl:variable name="style">
   <xsl:for-each select=".//*[@style]">
    <xsl:variable name="id" select="generate-id()" />
    <xsl:variable name="matches" select="key('style',@style)" />
    <xsl:if test="1 &lt; count($matches) and generate-id($matches[1]) = $id">
     <xsl:text>
     .st-</xsl:text><xsl:value-of select="$id" /><xsl:text> {</xsl:text>
     <xsl:text>
      </xsl:text><xsl:value-of select="@style" />
     <xsl:text>
     }</xsl:text>
    </xsl:if>
   </xsl:for-each> 
  </xsl:variable>

  <xsl:element name="svg" namespace="http://www.w3.org/2000/svg">
   <!-- Copy attributes, and ... -->
   <xsl:apply-templates select="@*" />
   <!-- set preserveAspectRatio="none" -->
   <xsl:attribute name="preserveAspectRatio">none</xsl:attribute>
   <!-- set width="{$width}" -->
   <xsl:attribute name="width"><xsl:value-of select="$width" /></xsl:attribute>

   <!-- generate defs/style if $style is not empty -->
   <xsl:if test=" normalize-space($style)">
    <xsl:element name="defs" namespace="http://www.w3.org/2000/svg">
    <xsl:element name="style" namespace="http://www.w3.org/2000/svg">
     <xsl:value-of select="$style" />
     <xsl:text>
     </xsl:text>
    </xsl:element>
    </xsl:element>
   </xsl:if>

   <!-- Copy child nodes -->
   <xsl:apply-templates select="node()" />
  </xsl:element>
 </xsl:template>

 <xsl:template match="/comment()[contains(.,'version 2 of the License')]">
  <!-- replace 2 with 3 -->
  <xsl:comment><xsl:value-of select="substring-before(.,'2')" />3<xsl:value-of select="substring-after(.,'2')" /></xsl:comment>
 </xsl:template>

 <xsl:template match="/comment()[contains(.,'Copyright')]">
  <xsl:copy><xsl:apply-templates select="text()"/></xsl:copy>
  <xsl:comment>    Copyright © 2017 Stan Livitski						</xsl:comment>
 </xsl:template>

 <xsl:template match="comment()[contains(.,'Generator:')]|comment()[contains(.,'bellot@')]">
  <!-- omit these elements -->
 </xsl:template>

 <xsl:template match="/svg//*[@style]">
  <xsl:copy>
   <!-- Copy attributes except 'style' -->
   <xsl:apply-templates select="@*[name() != 'style']" />

   <!-- If 'style' is included in /svg/defs/style ... -->
   <xsl:variable name="style-matches" select="key('style',@style)" />
   <xsl:choose>
    <xsl:when test="1 &lt; count($style-matches)">
    <!-- set the 'class' attribute to reference that style -->
     <xsl:attribute name="class">
      <xsl:value-of select="concat('st-',generate-id($style-matches[1]))" />
      <xsl:if test="@class">
       <xsl:value-of select="concat(' ',@class)" />
      </xsl:if>
     </xsl:attribute>
    </xsl:when>
    <!-- else, copy the 'style' attribute -->
    <xsl:otherwise>
     <xsl:attribute name="style"><xsl:value-of select="@style" /></xsl:attribute>
    </xsl:otherwise>
   </xsl:choose>

   <!-- Copy child nodes -->
   <xsl:apply-templates select="node()"/>
  </xsl:copy>
 </xsl:template>

 <xsl:template match="@*|node()">
  <xsl:copy><xsl:apply-templates select="@*|node()"/></xsl:copy>
 </xsl:template>

</xsl:transform>