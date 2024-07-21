      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  13.04.2005                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRVB0772.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRVB0772.CPv  $                                
      *
      *   Rev 2.1   May 23 2005 14:23:52   t13ae27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *                                                                 
      *   Rev 2.0   Apr 13 2005 11:13:28   t13ex22                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
                                                                        
      *---------------------------------------------------------------* 
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS                         
      *            DATUM   :  JANUAR 2003                               
      *            KOPF AUS:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRVB0772  FUNKTION ZU UPVB0130                  
      *    --------------------------------------------------------   * 
      *    FUNKTION VERGLEICHT BERECHNET EINHEITEN IM BASIS- UND IM     
      *    STEIGERUNGSZEITRAUM. HERAUSGEWACHSENE DIREKTIONEN WERDEN IM  
      *    BASISZEITRAUM SO BEHANDELT, ALS WÄREN SIE NIE ENTHALTEN      
      *    GEWESEN. IM STEIGERUNGSZEITRAUM ERFOLGT DIE BERECHNUNG ALS   
      *    WÄREN DIE DIREKTIONEN VOLLSTÄNDIG ENTHALTEN.                 
      *                                                                 
      *    ACHTUNG: DIESE FUNKTION FÜHRT AUF MONATSEBENE EINE NULL-     
      *    SETZUNG NEGATIVER DIFFERENZEN DURCH.D.H., WENN DIE DIFFERENZ 
      *    DER EINHEITEN ZWISCHEN ALTER UND NEUER DIREKTION NEGATIV     
      *    WÜRDE, SO WÜRDE DIES ALS NULL BETRACHTET.                    
      *                                                                 
      *                                                                 
      *    FUNKTION ENTSPRICHT DRVB0698 BIS AUF FOLGENDE ÄNDERUNG:      
      *    DIE BERECHNUNG DER EINHEITEN ERFOLGT NICHT AUF BASIS DER     
      *    WERTE AUS DER GEEV, SONDERN AUF BASIS DER WERTE AUS DER      
      *    PARTEH.                                                      
      *                                                                 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: MUND                                             * 
      *    DATUM:    13.04.2005                                       * 
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AAS00531!23.04.05! 01 ! NEU                       ! MUND     *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL            
      *    PRO UPRO ÜBERGEBEN WERDEN.                                   
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN         
      *                (IN SEPARATEN COPYS PRO FUNKTION)                
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRVB0772.                                                 
           05  PI-DRVB0772-DATEN.                                       
               10 PI-DRVB0772-MA-ST-NR         PIC S9(009) COMP.        
               10 PI-DRVB0772-BASIS-VON        PIC  X(010).             
               10 PI-DRVB0772-BASIS-BIS        PIC  X(010).             
               10 PI-DRVB0772-BASIS-ENDE       PIC  X(010).             
               10 PI-DRVB0772-STEIGERUNG-VON   PIC  X(010).             
               10 PI-DRVB0772-STEIGERUNG-BIS   PIC  X(010).             
               10 PI-DRVB0772-PROGRAMM         PIC  X(008).             
               10 PI-DRVB0772-WB-TYP           PIC S9(004) COMP.        
               10 PI-DRVB0772-WERTUNGS-TYP     PIC S9(004) COMP.        
               10 PI-DRVB0772-GESELL           PIC S9(004) COMP.        
060505         10 PI-DRVB0772-UMSATZ-KZ        PIC S9(004) COMP.        
                                                                        
      *                                                                 
       01  PO-DRVB0772.                                                 
           05  PO-DRVB0772-RC                  PIC  X(003).             
               88 PO-DRVB0772-RC-OK                        VALUE 'OK '. 
               88 PO-DRVB0772-RC-NOT-FOUND                 VALUE 'NFD'. 
           05  PO-DRVB0772-MA-ST-NR            PIC S9(009) COMP.        
           05  PO-DRVB0772-EH-BASIS            PIC S9(09)V99 COMP.      
           05  PO-DRVB0772-EH-STEIGERUNG       PIC S9(09)V99 COMP.      
           05  PO-DRVB0772-NEUE-DIREKTIONEN    PIC  9(001).             
               88 PO-DRVB0772-ND-VORHANDEN                 VALUE 1.     
               88 PO-DRVB0772-ND-NICHT-VORHANDEN           VALUE 2.     
                                                                        
