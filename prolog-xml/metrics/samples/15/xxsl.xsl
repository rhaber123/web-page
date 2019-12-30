<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="/">
     <table border="1" cellpadding="6">
          <tr>
               <th colspan="2">Axis: child</th>
          </tr>
          <tr>
               <th>Element</th>
               <th>Node-set</th>
          </tr>
          <xsl:for-each select="/source//*">
               <xsl:call-template name="print"/>
          </xsl:for-each>
     </table>
</xsl:template>

<xsl:template name="print">
     <tr>
          <td>
               <xsl:value-of select="name()"/>
               <xsl:text> id = </xsl:text>
               <xsl:value-of select="./@id"/>
          </td>
          <td>
               <xsl:for-each select="child::*">
                    <xsl:value-of select="./@id"/>
                    <xsl:text> </xsl:text>
               </xsl:for-each>
          </td>
     </tr>
</xsl:template>


</xsl:stylesheet>