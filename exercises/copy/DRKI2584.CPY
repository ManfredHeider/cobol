      * --------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  29.03.2012                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2584.CPY  $                                         

                                                                        
      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2584.CPv  $                            
      *
      *   Rev 2.2   Dec 17 2012 11:13:46   t13ae49
      *FKI01384: Wartungsauftrag NKDA - VN-IDENT wg. vertrauschte K
      *TO-I anpassen
      *                                                                 
      *   Rev 2.1   Sep 20 2012 10:47:40   t13ae49                      
      *AKI00886: Host - neues Design Kundenaktualisierung - Hauptpr     
      *ogramm                                                           
      *                                                                 
      *   Rev 2.0   Mar 29 2012 11:25:19 T13AE49                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2584                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2584                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         13.12.2012 / 10:20:00 / T13AE49    
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
      *    FÜR DIE FUNKTION DRKI2584                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2584.                                                 
           05  PI-DRKI2584-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2584-KDKLAUFTERMIN   PIC  X(026).                
             10 PI-DRKI2584-KDKPARTGESELLSCHID   PIC S9(4) COMP.        
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2584.                                                 
           05  PO-DRKI2584-RC                     PIC  X(003).          
               88 PO-DRKI2584-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2584-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
           05  PO-DRKI2584-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PO_DATEN%                                             
             10 PO-DRKI2584-KDKJOBNAME   PIC  X(0008).                  
             10 PO-DRKI2584-NI-KDKJOBNAME   PIC S9(4) COMP.             
             10 PO-DRKI2584-KDKPRUEFWERT   PIC S9(9) COMP.              
             10 PO-DRKI2584-NI-KDKPRUEFWERT   PIC S9(4) COMP.           
             10 PO-DRKI2584-KDKPRUEFWERT2   PIC S9(9) COMP.             
             10 PO-DRKI2584-NI-KDKPRUEFWERT2   PIC S9(4) COMP.          
             10 PO-DRKI2584-KDKPRUEFWERT3   PIC S9(9) COMP.             
             10 PO-DRKI2584-NI-KDKPRUEFWERT3   PIC S9(4) COMP.          
             10 PO-DRKI2584-KDKPRUEFWERT4   PIC S9(9) COMP.             
             10 PO-DRKI2584-NI-KDKPRUEFWERT4   PIC S9(4) COMP.          
             10 PO-DRKI2584-KDKSATZANZAHL   PIC S9(9) COMP.             
             10 PO-DRKI2584-NI-KDKSATZANZAHL   PIC S9(4) COMP.          
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
