<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE>
          <xsl:for-each select="//word">
               <xsl:sort case-order="upper-first" select="@id"/>
               <TR>
                    <TH>
                         <xsl:value-of select="@id"/>
                    </TH>
               </TR>
          </xsl:for-each>
     </TABLE>
</xsl:template>


</xsl:stylesheet>