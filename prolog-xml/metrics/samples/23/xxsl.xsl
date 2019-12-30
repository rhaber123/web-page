<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="color">
     <TABLE>
          <TR>
               <TD>
                    <xsl:attribute name="style">
                         <xsl:text>color:</xsl:text>
                         <xsl:value-of select="."/>
                    </xsl:attribute>
                    <xsl:value-of select="."/>
               </TD>
          </TR>
     </TABLE>
</xsl:template>


</xsl:stylesheet>