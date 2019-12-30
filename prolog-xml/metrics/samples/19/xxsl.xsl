<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE>
          <xsl:for-each select="//name">
               <xsl:sort order="ascending" select="."/>
               <TR>
                    <TH>
                         <xsl:value-of select="."/>
                    </TH>
               </TR>
          </xsl:for-each>
     </TABLE>
</xsl:template>


</xsl:stylesheet>