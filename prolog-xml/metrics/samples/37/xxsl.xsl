<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE border="1">
          <TR>
               <TH>text</TH>
               <TH>number</TH>
          </TR>
          <xsl:for-each select="//text">
               <TR>
                    <TD>
                         <xsl:value-of select="."/>
                    </TD>
                    <TD>
                         <xsl:value-of select="number()"/>
                    </TD>
               </TR>
          </xsl:for-each>
     </TABLE>
</xsl:template>


</xsl:stylesheet>