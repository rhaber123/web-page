<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE border="1">
          <TR>
               <TH colspan="3">
                    <xsl:value-of select="//text"/>
               </TH>
          </TR>
          <TR>
               <TH>string</TH>
               <TH>starts-with</TH>
               <TH>contains</TH>
          </TR>
          <xsl:for-each select="//string">
               <TR>
                    <TD>
                         <xsl:value-of select="."/>
                    </TD>
                    <TD>
                         <xsl:value-of select="starts-with(//text,.)"/>
                    </TD>
                    <TD>
                         <xsl:value-of select="contains(//text,.)"/>
                    </TD>
               </TR>
          </xsl:for-each>
     </TABLE>
</xsl:template>


</xsl:stylesheet>