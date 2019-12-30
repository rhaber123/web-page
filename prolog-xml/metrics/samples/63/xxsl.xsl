<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="p">
     <DIV>
          <B>
               <xsl:text>copy-of : </xsl:text>
          </B>
          <xsl:copy-of select="."/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>copy : </xsl:text>
          </B>
          <xsl:copy/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>value-of : </xsl:text>
          </B>
          <xsl:value-of select="."/>
     </DIV>
</xsl:template>


</xsl:stylesheet>