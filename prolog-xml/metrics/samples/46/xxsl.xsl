<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="text">
     <P>
          <xsl:choose>
               <xsl:when test='lang("cs")'>
                    <xsl:text>Czech: </xsl:text>
               </xsl:when>
               <xsl:when test='lang("en")'>
                    <xsl:text>English: </xsl:text>
               </xsl:when>
               <xsl:when test='lang("de")'>
                    <xsl:text>German: </xsl:text>
               </xsl:when>
          </xsl:choose>
          <xsl:value-of select="."/>
     </P>
</xsl:template>


</xsl:stylesheet>