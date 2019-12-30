<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="number">
     <P>
          <xsl:if test="true()">
               <xsl:text>true </xsl:text>
          </xsl:if>
          <xsl:if test="not(false())">
               <xsl:text>not false</xsl:text>
          </xsl:if>
     </P>
</xsl:template>


</xsl:stylesheet>