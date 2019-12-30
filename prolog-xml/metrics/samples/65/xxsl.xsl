<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <TABLE border="1">
          <TR>
               <TH> . </TH>
               <TH>current()</TH>
          </TR>
          <xsl:apply-templates select="//AAA"/>
     </TABLE>
</xsl:template>

<xsl:template match="AAA">
     <TR>
          <TD>
               <xsl:value-of select="./@name"/>
          </TD>
          <TD>
               <xsl:value-of select="current()/@name"/>
          </TD>
     </TR>
     <TR>
          <TD>
               <xsl:apply-templates select="BBB[./@name='first']"/>
          </TD>
          <TD>
               <xsl:apply-templates select="BBB[current()/@name='first']"/>
          </TD>
     </TR>
</xsl:template>


</xsl:stylesheet>