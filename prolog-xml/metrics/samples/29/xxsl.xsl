<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE BORDER="1">
          <TR>
               <TH>Number</TH>
               <TH>text</TH>
          </TR>
          <xsl:for-each select="//chapter">
               <TR>
                    <TD>
                         <xsl:number/>
                    </TD>
                    <TD>
                         <xsl:value-of select="./text()"/>
                    </TD>
               </TR>
          </xsl:for-each>
     </TABLE>
</xsl:template>


</xsl:stylesheet>