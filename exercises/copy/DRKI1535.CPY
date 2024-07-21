      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Mai 2002                                  
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH                
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1535.CPY  $                                 00010000

      *    $Revision:   2.7  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1535.CPv  $                            
      *
      *   Rev 2.7   Nov 13 2007 09:28:46   t13ex50
      *AKI00686: FOZVERTRAGSTYP    versorgen
      *                                                                 
      *   Rev 2.6   Jul 05 2006 18:04:14   t13ex39                      
      *FKI01038: Tabellw GUTHABENZAHLUNG                                
      *                                                                 
      *   Rev 2.5   Jan 16 2006 17:58:52   t13ex39                      
      *AKI00494: Änderung der Lieferung DeuBa-Sparpläne                 
      *                                                                 
      *   Rev 2.2   Mar 30 2005 12:33:08   t13ex39                      
      *FKI00928: GUTHABENZAHLUNG via Replikation                        
      *                                                                 
      *   Rev 2.1   Jul 10 2003 13:30:20   t13ex39                      
      *AKI00327.UE005 DB-PRODUKTE                                       
      *                                                                 
      *   Rev 2.0   Oct 16 2002 08:33:04   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1535                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1535                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   erzeugt mit genOZP - (c) PMSmicado  19.11.2002/18:08:24       
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00328!24.03.03!01.0! Neu                       !WHO       *  
      *         !        !    !                           !          *  
      * -------------------------------------------------------------*  
      * FKI00928!22.03.05!01.3! zus. Atrribute DRKI1535   !WHO       *  
      *         !        !    ! FOZDATBEGINNAUS           !          *  
      *         !        !    ! FOZDATENDEAUS             !          *  
      *         !        !    !                           !          *  
      * -------------------------------------------------------------*  
      * AKI00494!26.01.06!01.4! Recompile wg. Änderung    !WHO       *  
      *         !        !    ! BLZ und Kontonr.          !          *  
      *         !        !    !                           !          *  
      * -------------------------------------------------------------*  
      * FKI01038!05.07.05!ZUS. ATTRIBUTE FÜR VSP          ! WHO      *  
      *         !        !                                !          *  
      * -------------------------------------------------------------*  
      * AKI00686!09.11.07!ZUS. ATTRIBUT VERTRAGSTYP       !SCHLEICHER*  
      *         !        !                                !          *  
      *         !        !                                !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER            
      *    FÜR DIE FUNKTION DRKI1535                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01 PI-DRKI1535.                                                  
          05 PI-DRKI1535-PROGRAMM               PIC X(008).             
          05 PI-DRKI1535-UFKT                   PIC 9(002).             
          05 PI-DRKI1535-KEY.                                           
             10 PI-DRKI1535-KEY-VON              PIC X(13).             
             10 PI-DRKI1535-KEY-BIS              PIC X(13).             
      *                                                                 
       01 PO-DRKI1535.                                                  
                                                                        
          05 PO-DRKI1535-RC                     PIC  X(003).            
             88 PO-DRKI1535-RC-OK                    VALUE 'OK '.       
             88 PO-DRKI1535-RC-EOF                   VALUE 'EOF'.       
                                                                        
          05 PO-DRKI1535-KEY.                                           
             10 PO-DRKI1535-FOZFONDID            PIC S9(4) COMP.        
             10 PO-DRKI1535-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PO-DRKI1535-FOZLFDNR             PIC S9(4) COMP.        
             10 PO-DRKI1535-FOZVERTRAGSID        PIC X(13).             
                                                                        
          05  PO-DRKI1535-DATEN.                                        
                                                                        
                                                                        
             10 PO-DRKI1535-FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) COMP-3. 
             10 PO-DRKI1535-FOZAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.   
             10 PO-DRKI1535-FOZBANKBLZ   PIC X(11).                     
             10 PO-DRKI1535-FOZBANKKONTONR   PIC X(18).                 
             10 PO-DRKI1535-FOZBANKNAME   PIC X(20).                    
             10 PO-DRKI1535-FOZBEGDATDYNAM   PIC X(10).                 
             10 PO-DRKI1535-FOZBUCHUNGSTEXTID   PIC S9(4) COMP.         
             10 PO-DRKI1535-FOZDATBEGINNAUS   PIC X(10).                
             10 PO-DRKI1535-FOZDATENDEAUS   PIC X(10).                  
             10 PO-DRKI1535-FOZDATVERTRAUFLOE   PIC X(10).              
             10 PO-DRKI1535-FOZDYNAMIK   PIC S9(1)V9(2) COMP-3.         
             10 PO-DRKI1535-FOZEINAUSDATUM   PIC S9(4) COMP.            
             10 PO-DRKI1535-FOZEINAUSTYP   PIC X(1).                    
             10 PO-DRKI1535-FOZEINZAHFRISTEND   PIC X(10).              
             10 PO-DRKI1535-FOZKONTOINHABER   PIC X(27).                
             10 PO-DRKI1535-FOZKOSTENPROZ   PIC S9(1)V9(2) COMP-3.      
             10 PO-DRKI1535-FOZKUENDMERKMAL   PIC X(1).                 
             10 PO-DRKI1535-FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) COMP-3. 
             10 PO-DRKI1535-FOZSPARPLANEROEF   PIC X(10).               
             10 PO-DRKI1535-FOZSPARPLANSCHLUSS   PIC X(10).             
             10 PO-DRKI1535-FOZURSPARRATE   PIC S9(4)V9(2) COMP-3.      
             10 PO-DRKI1535-FOZURSPARRATEWKZ   PIC X(1).                
             10 PO-DRKI1535-FOZURSPRSPARLST   PIC S9(6)V9(2) COMP-3.    
             10 PO-DRKI1535-FOZURSPRSPARLSTWKZ   PIC X(1).              
             10 PO-DRKI1535-FOZVERTRAGSTYP   PIC X(2).                  
             10 PO-DRKI1535-FOZVERTRLAUFZEIT   PIC S9(4) COMP.          
             10 PO-DRKI1535-FOZZAHLUNG   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI1535-FOZZAHLUNGSWEISEID   PIC S9(4) COMP.        
             10 PO-DRKI1535-FOZZAHLUNGWKZ   PIC X(1).                   
             10 PO-DRKI1535-FOZZLASTSCHRRUECK   PIC S9(4) COMP.         
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
      *                                                                 
                                                                        
