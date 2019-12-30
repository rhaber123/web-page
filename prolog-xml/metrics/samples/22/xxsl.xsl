<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <xsl:for-each select="//text">
          <xsl:element name="{@size}">
               <xsl:value-of select="."/>
          </xsl:element>
     </xsl:for-each>
</xsl:template>


</xsl:stylesheet>