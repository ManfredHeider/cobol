      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Mai 2002                                  
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH                
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1488.CPY  $                                         

      *    $Revision:   2.5  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1488.CPv  $                                
      *
      *   Rev 2.5   Nov 13 2007 09:28:20   t13ex50
      *AKI00686: FOZVERTRAGSTYP    versorgen
      *                                                                 
      *   Rev 2.0   Oct 16 2002 08:32:38   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1488                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1488                    * 
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
      *    FÜR DIE FUNKTION DRKI1488                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI1488.                                                 
           05  PI-DRKI1488-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI1488-FOZFONDID   PIC S9(4) COMP.                 
             10 PI-DRKI1488-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI1488-FOZLFDNR   PIC S9(4) COMP.                  
             10 PI-DRKI1488-FOZVERTRAGSID   PIC X(13).                  
                                                                        
      *                                                                 
       01  PO-DRKI1488.                                                 
           05  PO-DRKI1488-RC                 PIC  X(003).              
               88 PO-DRKI1488-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI1488-RC-NOT-FOUND                 VALUE 'NFD'. 
      *                                                                 
           05  PO-DRKI1488-DATEN.                                       
                                                                        
                                                                        
             10 PO-DRKI1488-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3. 
             10 PO-DRKI1488-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.   
             10 PO-DRKI1488-FOZBANKBLZ   PIC X(11).                     
             10 PO-DRKI1488-FOZBANKKONTONR   PIC X(18).                 
             10 PO-DRKI1488-FOZBANKNAME   PIC X(20).                    
             10 PO-DRKI1488-FOZBEGDATDYNAM   PIC X(10).                 
             10 PO-DRKI1488-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.         
             10 PO-DRKI1488-FOZDATBEGINNAUS   PIC X(10).                
             10 PO-DRKI1488-FOZDATENDEAUS   PIC X(10).                  
             10 PO-DRKI1488-FOZDATVERTRAUFLOE   PIC X(10).              
             10 PO-DRKI1488-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.         
             10 PO-DRKI1488-FOZEINAUSDATUM   PIC S9(4) COMP.            
             10 PO-DRKI1488-FOZEINAUSTYP   PIC X(1).                    
             10 PO-DRKI1488-FOZEINZAHFRISTEND   PIC X(10).              
             10 PO-DRKI1488-FOZKONTOINHABER   PIC X(27).                
             10 PO-DRKI1488-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.      
             10 PO-DRKI1488-FOZKUENDMERKMAL   PIC X(1).                 
             10 PO-DRKI1488-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3. 
             10 PO-DRKI1488-FOZSPARPLANEROEF   PIC X(10).               
             10 PO-DRKI1488-FOZSPARPLANSCHLUSS   PIC X(10).             
             10 PO-DRKI1488-FOZURSPARRATE   PIC S9(4)V9(2) COMP-3.      
             10 PO-DRKI1488-FOZURSPARRATEWKZ   PIC X(1).                
             10 PO-DRKI1488-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.    
             10 PO-DRKI1488-FOZURSPRSPARLSTWKZ   PIC X(1).              
             10 PO-DRKI1488-FOZVERTRAGSTYP   PIC X(2).                  
             10 PO-DRKI1488-FOZVERTRLAUFZEIT   PIC S9(4) COMP.          
             10 PO-DRKI1488-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI1488-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.        
             10 PO-DRKI1488-FOZZAHLUNGWKZ   PIC X(1).                   
             10 PO-DRKI1488-FOZZLASTSCHRRUECK   PIC S9(4) COMP.         
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
