      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Mai 2002                                  
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH                
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1489.CPY  $                                         

      *    $Revision:   2.5  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1489.CPv  $                                
      *
      *   Rev 2.5   Nov 13 2007 09:28:32   t13ex50
      *AKI00686: FOZVERTRAGSTYP    versorgen
      *                                                                 
      *   Rev 2.0   Oct 16 2002 08:32:44   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1489                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1489                    * 
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
      *    FÜR DIE FUNKTION DRKI1489                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI1489.                                                 
           05  PI-DRKI1489-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI1489-FOZFONDID   PIC S9(4) COMP.                 
             10 PI-DRKI1489-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI1489-FOZLFDNR   PIC S9(4) COMP.                  
             10 PI-DRKI1489-FOZVERTRAGSID   PIC X(13).                  
                                                                        
                                                                        
           05  PI-DRKI1489-DATEN.                                       
                                                                        
                                                                        
             10 PI-DRKI1489-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3. 
             10 PI-DRKI1489-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.   
             10 PI-DRKI1489-FOZBANKBLZ   PIC X(11).                     
             10 PI-DRKI1489-FOZBANKKONTONR   PIC X(18).                 
             10 PI-DRKI1489-FOZBANKNAME   PIC X(20).                    
             10 PI-DRKI1489-FOZBEGDATDYNAM   PIC X(10).                 
             10 PI-DRKI1489-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.         
             10 PI-DRKI1489-FOZDATBEGINNAUS   PIC X(10).                
             10 PI-DRKI1489-FOZDATENDEAUS   PIC X(10).                  
             10 PI-DRKI1489-FOZDATVERTRAUFLOE   PIC X(10).              
             10 PI-DRKI1489-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.         
             10 PI-DRKI1489-FOZEINAUSDATUM   PIC S9(4) COMP.            
             10 PI-DRKI1489-FOZEINAUSTYP   PIC X(1).                    
             10 PI-DRKI1489-FOZEINZAHFRISTEND   PIC X(10).              
             10 PI-DRKI1489-FOZKONTOINHABER   PIC X(27).                
             10 PI-DRKI1489-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.      
             10 PI-DRKI1489-FOZKUENDMERKMAL   PIC X(1).                 
             10 PI-DRKI1489-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3. 
             10 PI-DRKI1489-FOZSPARPLANEROEF   PIC X(10).               
             10 PI-DRKI1489-FOZSPARPLANSCHLUSS   PIC X(10).             
             10 PI-DRKI1489-FOZURSPARRATE   PIC S9(4)V9(2) COMP-3.      
             10 PI-DRKI1489-FOZURSPARRATEWKZ   PIC X(1).                
             10 PI-DRKI1489-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.    
             10 PI-DRKI1489-FOZURSPRSPARLSTWKZ   PIC X(1).              
             10 PI-DRKI1489-FOZVERTRAGSTYP   PIC X(2).                  
             10 PI-DRKI1489-FOZVERTRLAUFZEIT   PIC S9(4) COMP.          
             10 PI-DRKI1489-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI1489-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.        
             10 PI-DRKI1489-FOZZAHLUNGWKZ   PIC X(1).                   
             10 PI-DRKI1489-FOZZLASTSCHRRUECK   PIC S9(4) COMP.         
                                                                        
      *                                                                 
       01  PO-DRKI1489.                                                 
           05  PO-DRKI1489-RC                 PIC  X(003).              
               88 PO-DRKI1489-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI1489-RC-DUPLICATE-VALUE           VALUE 'DVA'. 
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
