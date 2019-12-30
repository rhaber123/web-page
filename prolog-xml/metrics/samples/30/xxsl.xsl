<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE>
          <xsl:for-each select="//n">
               <TR>
                    <TD>
                         <xsl:number value="position()" format="1. "/>
                         <xsl:value-of select="."/>
                    </TD>
               </TR>
          </xsl:for-each>
     </TABLE>
</xsl:template>


</xsl:stylesheet>