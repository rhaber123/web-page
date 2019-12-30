<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <DIV>
          <B>
               <xsl:text>//AAA : </xsl:text>
          </B>
          <xsl:value-of select="count(//AAA)"/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>//CCC : </xsl:text>
          </B>
          <xsl:value-of select="count(//CCC)"/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>//AAA/CCC : </xsl:text>
          </B>
          <xsl:value-of select="count(//AAA/CCC)"/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>//CCC[text()]) : </xsl:text>
          </B>
          <xsl:value-of select="count(//CCC[text()])"/>
     </DIV>
</xsl:template>


</xsl:stylesheet>