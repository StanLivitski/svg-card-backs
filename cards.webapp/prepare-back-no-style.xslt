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
   remove misleading information.
*****************************************************************************
-->
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
 xmlns:svg="http://www.w3.org/2000/svg"
>

 <xsl:param name="width" select="'171.5900947pt'" />

 <xsl:output method="xml" indent="yes" />

 <xsl:template match="/svg">
  <xsl:element name="svg" namespace="http://www.w3.org/2000/svg">
   <xsl:apply-templates select="@*" /><!--
 set preserveAspectRatio="none" --><xsl:attribute name="preserveAspectRatio">none</xsl:attribute><!--
 set width="{$width}" --><xsl:attribute name="width"><xsl:value-of select="$width" /></xsl:attribute><!--
  --><xsl:apply-templates select="node()" /></xsl:element>
 </xsl:template>

 <xsl:template match="/comment()[contains(.,'Copyright')]">
  <xsl:copy><xsl:apply-templates select="text()"/></xsl:copy>
  <xsl:comment>    Copyright © 2017 Stan Livitski						</xsl:comment>
 </xsl:template>

 <xsl:template match="/comment()[contains(.,'version 2 of the License')]">
  <xsl:comment><!-- replace 2 with 3
--><xsl:value-of select="substring-before(.,'2')" />3<xsl:value-of select="substring-after(.,'2')" /></xsl:comment>
 </xsl:template>

 <xsl:template match="/comment()[contains(.,'This set of SVG files is')]">
  <xsl:comment>    This set of SVG files contains images of playing cards' backs		</xsl:comment>
 </xsl:template>

 <xsl:template match="/comment()[contains(.,'and backs of cards completely designed in SVG.')]">
  <xsl:comment>    adapted from version 1.1 of the upstream project			</xsl:comment>
  <xsl:comment>    &lt;https://sourceforge.net/projects/svg-cards/&gt;				</xsl:comment>
 </xsl:template>

 <xsl:template match="comment()[contains(.,'Generator:')]|comment()[contains(.,'bellot@')]"><!-- omit these elements
 --></xsl:template>

 <xsl:template match="@*|node()"><xsl:copy><xsl:apply-templates select="@*|node()"/></xsl:copy></xsl:template>

</xsl:transform>