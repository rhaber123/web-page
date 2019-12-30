<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <xsl:for-each select="//number">
          <xsl:value-of select="."/>
          <xsl:choose>
               <xsl:when test="position()=last()">
                    <xsl:text> = </xsl:text>
               </xsl:when>
               <xsl:otherwise>
                    <xsl:text> + </xsl:text>
               </xsl:otherwise>
          </xsl:choose>
     </xsl:for-each>
     <xsl:value-of select="sum(//number)"/>
</xsl:template>


</xsl:stylesheet>