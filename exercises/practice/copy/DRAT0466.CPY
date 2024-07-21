      **--------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  08.09.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRAT0466.CPY  $                                      


      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRAT0466.CPv  $                            
      *
      *   Rev 2.2   Sep 09 2010 11:10:54   t13ae49
      *AKI00988: Ablaufkontrolle wegen Preisakt. anpassen - DTA-Kon
      *. PRG 2 anpassen
      *
      *   Rev 2.1   Sep 09 2010 08:57:12   t13ae49
      *AKI00988: Ablaufkontrolle wegen Preisakt. anpassen - DTA-Kon
      *. PRG 2 anpassen
      *                                                                 
      *   Rev 2.0   Sep 8 2010 09:39:11 T13AE49                         
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRAT0466                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRAT0466                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         08.09.2010 / 09:39:11 / T13AE49    
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER            
      *    FšR DIE FUNKTION DRAT0466                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRAT0466.                                                 
           05  PI-DRAT0466-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRAT0466-ABKJOBNAME   PIC  X(0008).                  
             10 PI-DRAT0466-ABKLFDNR   PIC S9(9) COMP.                  
             10 PI-DRAT0466-ABKMANDANTNR   PIC S9(9) COMP.              
             10 PI-DRAT0466-ABKPARTGESID   PIC S9(4) COMP.              
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRAT0466-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRAT0466-ABKABGLSTRSATZKZ   PIC  X(0001).            
             10 PI-DRAT0466-ABKBEGABLAUF   PIC  X(026).                 
             10 PI-DRAT0466-ABKBEGAKTU   PIC  X(026).                   
             10 PI-DRAT0466-ABKBEGEINARB   PIC  X(026).                 
             10 PI-DRAT0466-ABKENDAKTU   PIC  X(026).                   
             10 PI-DRAT0466-ABKENDEINARB   PIC  X(026).                 
             10 PI-DRAT0466-ABKKATALOGDATUM   PIC  X(010).              
             10 PI-DRAT0466-ABKPARTNERDATUM   PIC  X(010).              
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRAT0466.                                                 
           05  PO-DRAT0466-RC                     PIC  X(003).          
               88 PO-DRAT0466-RC-OK                  VALUE 'OK '.       
               88 PO-DRAT0466-RC-DUPLICATE-VALUE VALUE 'DVA'.           
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
