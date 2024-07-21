      * --------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  29.03.2012                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2583.CPY  $                                         

                                                                        
      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2583.CPv  $                            
      *
      *   Rev 2.2   Dec 17 2012 11:13:42   t13ae49
      *FKI01384: Wartungsauftrag NKDA - VN-IDENT wg. vertrauschte K
      *TO-I anpassen
      *                                                                 
      *   Rev 2.1   Sep 20 2012 10:47:34   t13ae49                      
      *AKI00886: Host - neues Design Kundenaktualisierung - Hauptpr     
      *ogramm                                                           
      *                                                                 
      *   Rev 2.0   Mar 29 2012 11:25:17 T13AE49                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2583                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2583                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         13.12.2012 / 10:19:57 / T13AE49    
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER            
      *    FÜR DIE FUNKTION DRKI2583                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2583.                                                 
           05  PI-DRKI2583-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2583-KDKLAUFTERMIN   PIC  X(026).                
             10 PI-DRKI2583-KDKPARTGESELLSCHID   PIC S9(4) COMP.        
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI2583-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI2583-KDKJOBNAME   PIC  X(0008).                  
             10 PI-DRKI2583-KDKPRUEFWERT   PIC S9(9) COMP.              
             10 PI-DRKI2583-KDKPRUEFWERT2   PIC S9(9) COMP.             
             10 PI-DRKI2583-KDKPRUEFWERT3   PIC S9(9) COMP.             
             10 PI-DRKI2583-KDKPRUEFWERT4   PIC S9(9) COMP.             
             10 PI-DRKI2583-KDKSATZANZAHL   PIC S9(9) COMP.             
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2583.                                                 
           05  PO-DRKI2583-RC                     PIC  X(003).          
               88 PO-DRKI2583-RC-OK                  VALUE 'OK '.       
               88 PO-DRKI2583-RC-DUPLICATE-VALUE VALUE 'DVA'.           
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
