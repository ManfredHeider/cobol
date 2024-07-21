      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0038.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0038.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 16:53:12   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Nov 04 1995 08:17:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: DPAT0038                                      * 
      *    --------------------------------------------------------   * 
      *    BATCHRAHMEN: FEHLERAUFBEREITUNG UND -AUSGABE (DISPLAYS)    * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: SCHÖLLHAMMER                                     * 
      *    DATUM:    19.08.1994                                       * 
      ***************************************************************** 
      *    VERSION   1    14.09.94 16:35:20
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AFVB0144!19.08.94!    ! NEU                       ! SCHÖLLH. *  
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  
      * AFPA0012!06.11.97! 02 ! AUSGABE D-ERR-SCHLUESSEL  ! AUERBACH *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      * ROUTINEN: - BR99-SCHWERER-FEHLER-DISPLAY                        
      *           - UR82-D-ERR-DISPLAY                                  
      *                                                                 
      *------------------------------------------------------------*    
       BR99-SCHWERER-FEHLER-DISPLAY SECTION.                            
       BR99-ANF.                                                        
      *------------------------------------------------------------*    
      * INPUT : ERR-ERROR-BEREICH:                                 *    
      * OUTPUT: FEHLERBILD (ERRIPRO) MIT FEHLERINFORMATIONEN       *    
      * VERARBEITUNG:                                              *    
      *   - AUFBEREITUNG DER ERROR-SCHNITTSTELLE ABHÄNGIG VON      *    
      *     ERR-FEHLER-TYP UND ERR-AUFBEREITUNG                    *    
      *   - AUSGABE DER ERROR-INFORMATIONEN (DISPLAYS)             *    
      *------------------------------------------------------------*    
                                                                        
      *****  AUFBEREITEN FEHLER-INFORMATIONEN                           
                                                                        
           PERFORM UR81-ERR-AUFBER                                      
                                                                        
      *****  AUSGABE FEHLER-INFORMATIONEN                               
                                                                        
           PERFORM UR82-D-ERR-DISPLAY                                   
           .                                                            
       BR99-EXIT.                                                       
           EXIT.                                                        
      *                                                                 
      *------------------------------------------------------------*    
       UR82-D-ERR-DISPLAY SECTION.                                      
       UR82-ANF.                                                        
      *------------------------------------------------------------*    
      * INPUT : D-ERROR-BEREICH                                    *    
      * OUTPUT: FEHLERBILD (ERRIPRO) MIT FEHLERINFORMATIONEN       *    
      * VERARBEITUNG:                                              *    
      *   - AUSGABE DER ERROR-INFORMATIONEN (DISPLAYS)             *    
      *------------------------------------------------------------*    
                                                                        
           DISPLAY D-ERR-ZEILE-01                                       
           DISPLAY D-ERR-ZEILE-02                                       
           DISPLAY D-ERR-ZEILE-03                                       
           DISPLAY D-ERR-ZEILE-04                                       
           DISPLAY D-ERR-ZEILE-05                                       
           DISPLAY D-ERR-ZEILE-06                                       
           DISPLAY D-ERR-ZEILE-07                                       
           DISPLAY D-ERR-ZEILE-08                                       
           DISPLAY D-ERR-ZEILE-09                                       
           DISPLAY D-ERR-ZEILE-10                                       
           DISPLAY D-ERR-ZEILE-11                                       
           DISPLAY D-ERR-ZEILE-12                                       
           DISPLAY D-ERR-ZEILE-13                                       
           DISPLAY D-ERR-ZEILE-14                                       
           DISPLAY D-ERR-ZEILE-15                                       
           DISPLAY D-ERR-ZEILE-16                                       

061197*    AUSGABE 'SCHLüSSEL DES AKTUELL BEARBEITETEN EINGABESATZ'
061197*    DIE AUSGABE ERFOLGT VOR DEM 'VARIABLEN TRACEBEREICH'
061197     DISPLAY D-ERR-ZEILE-25                                       
061197     DISPLAY D-ERR-ZEILE-26                                       
061197     DISPLAY D-ERR-ZEILE-27                                       
061197     DISPLAY D-ERR-ZEILE-28                                       
061197     DISPLAY D-ERR-ZEILE-29                                       

           DISPLAY D-ERR-ZEILE-17                                       
           DISPLAY D-ERR-ZEILE-18                                       
           DISPLAY D-ERR-ZEILE-19                                       
           DISPLAY D-ERR-ZEILE-20                                       
           DISPLAY D-ERR-ZEILE-21                                       
           DISPLAY D-ERR-ZEILE-22                                       
           DISPLAY D-ERR-ZEILE-23                                       
           DISPLAY D-ERR-ZEILE-24                                       
           .                                                            
       UR82-EXIT.                                                       
           EXIT.                                                        
      *                                                                 
      ******** ENDE COPY-MEMBER (DPAT0038) *******************          
                                                                        
