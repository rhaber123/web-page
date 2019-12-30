<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <DIV>
          <xsl:for-each select="//BBB">
               <xsl:call-template name="printout"/>
          </xsl:for-each>
     </DIV>
     <DIV>
          <xsl:apply-templates select="//CCC"/>
     </DIV>
     <DIV>
          <xsl:apply-templates select="//AAA[last()]//CCC"/>
     </DIV>
</xsl:template>

<xsl:template match="CCC">
     <xsl:call-template name="printout"/>
</xsl:template>

<xsl:template name="printout">
     <xsl:if test="position()=1">
          <xsl:value-of select="name()"/>
     </xsl:if>
     <xsl:text>(</xsl:text>
     <xsl:value-of select="position()"/>
     <xsl:text>/</xsl:text>
     <xsl:value-of select="last()"/>
     <xsl:text>)</xsl:text>
</xsl:template>


</xsl:stylesheet>