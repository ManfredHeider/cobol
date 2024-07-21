      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Mai 2002                                  
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH                
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1491.CPY  $                                         

      *    $Revision:   2.5  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1491.CPv  $                                
      *
      *   Rev 2.5   Nov 13 2007 09:28:40   t13ex50
      *AKI00686: FOZVERTRAGSTYP    versorgen
      *                                                                 
      *   Rev 2.0   Oct 16 2002 08:32:58   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1491                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1491                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   erzeugt mit genOZP - (c) PMSmicado  08.11.2007/15:13:20       
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
      *    FÜR DIE FUNKTION DRKI1491                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  UPDATE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI1491.                                                 
           05  PI-DRKI1491-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI1491-FOZFONDID   PIC S9(4) COMP.                 
             10 PI-DRKI1491-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI1491-FOZLFDNR   PIC S9(4) COMP.                  
             10 PI-DRKI1491-FOZVERTRAGSID   PIC X(13).                  
                                                                        
                                                                        
           05  PI-DRKI1491-DATEN.                                       
                                                                        
                                                                        
             10 PI-DRKI1491-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3. 
             10 PI-DRKI1491-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.   
             10 PI-DRKI1491-FOZBANKBLZ   PIC X(11).                     
             10 PI-DRKI1491-FOZBANKKONTONR   PIC X(18).                 
             10 PI-DRKI1491-FOZBANKNAME   PIC X(20).                    
             10 PI-DRKI1491-FOZBEGDATDYNAM   PIC X(10).                 
             10 PI-DRKI1491-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.         
             10 PI-DRKI1491-FOZDATBEGINNAUS   PIC X(10).                
             10 PI-DRKI1491-FOZDATENDEAUS   PIC X(10).                  
             10 PI-DRKI1491-FOZDATVERTRAUFLOE   PIC X(10).              
             10 PI-DRKI1491-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.         
             10 PI-DRKI1491-FOZEINAUSDATUM   PIC S9(4) COMP.            
             10 PI-DRKI1491-FOZEINAUSTYP   PIC X(1).                    
             10 PI-DRKI1491-FOZEINZAHFRISTEND   PIC X(10).              
             10 PI-DRKI1491-FOZKONTOINHABER   PIC X(27).                
             10 PI-DRKI1491-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.      
             10 PI-DRKI1491-FOZKUENDMERKMAL   PIC X(1).                 
             10 PI-DRKI1491-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3. 
             10 PI-DRKI1491-FOZSPARPLANEROEF   PIC X(10).               
             10 PI-DRKI1491-FOZSPARPLANSCHLUSS   PIC X(10).             
             10 PI-DRKI1491-FOZURSPARRATE   PIC S9(4)V9(2) COMP-3.      
             10 PI-DRKI1491-FOZURSPARRATEWKZ   PIC X(1).                
             10 PI-DRKI1491-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.    
             10 PI-DRKI1491-FOZURSPRSPARLSTWKZ   PIC X(1).              
             10 PI-DRKI1491-FOZVERTRAGSTYP   PIC X(2).                  
             10 PI-DRKI1491-FOZVERTRLAUFZEIT   PIC S9(4) COMP.          
             10 PI-DRKI1491-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI1491-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.        
             10 PI-DRKI1491-FOZZAHLUNGWKZ   PIC X(1).                   
             10 PI-DRKI1491-FOZZLASTSCHRRUECK   PIC S9(4) COMP.         
                                                                        
                                                                        
      *                                                                 
       01  PO-DRKI1491.                                                 
           05  PO-DRKI1491-RC                 PIC  X(003).              
               88 PO-DRKI1491-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI1491-RC-NOT-FOUND                 VALUE 'NFD'. 
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
