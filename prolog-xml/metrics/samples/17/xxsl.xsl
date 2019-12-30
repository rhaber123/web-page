<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="AAA">
     <H3>
          <xsl:value-of select="name()"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@id"/>
     </H3>
     <TABLE border="1">
          <TR>
               <TH>full</TH>
               <TH>abbreviated</TH>
          </TR>
          <TR>
               <TD>
                    <xsl:text>child::BBB/attribute::id</xsl:text>
               </TD>
               <TD>
                    <xsl:text>BBB/@id</xsl:text>
               </TD>
          </TR>
          <TR>
               <TD>
                    <xsl:value-of select="child::BBB/attribute::id"/>
               </TD>
               <TD>
                    <xsl:value-of select="BBB/@id"/>
               </TD>
          </TR>
     </TABLE>
</xsl:template>


</xsl:stylesheet>