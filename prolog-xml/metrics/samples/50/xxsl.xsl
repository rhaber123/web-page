<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <DIV>
          <B>
               <xsl:text>Text: </xsl:text>
          </B>
          <xsl:value-of select="//text"/>
     </DIV>
     <B>
          <xsl:text>Text before </xsl:text>
          <xsl:value-of select="//string"/>
          <xsl:text>: </xsl:text>
     </B>
     <xsl:value-of select="substring-before(//text,//string)"/>
     <DIV>
          <B>
               <xsl:text>Text after </xsl:text>
               <xsl:value-of select="//string"/>
               <xsl:text>: </xsl:text>
          </B>
          <xsl:value-of select="substring-after(//text,//string)"/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>Text from position </xsl:text>
               <xsl:value-of select="//start"/>
               <xsl:text>: </xsl:text>
          </B>
          <xsl:value-of select="substring(//text,//start)"/>
     </DIV>
     <DIV>
          <B>
               <xsl:text>Text from position </xsl:text>
               <xsl:value-of select="//start"/>
               <xsl:text> of length </xsl:text>
               <xsl:value-of select="//end"/>
               <xsl:text>: </xsl:text>
          </B>
          <xsl:value-of select="substring(//text,//start,//end)"/>
     </DIV>
</xsl:template>


</xsl:stylesheet>