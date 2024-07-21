      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  November 1995                             
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRAT0096.CPY  $                                         

      *    $Revision:   2.7  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRAT0096.CPv  $                            
      *
      *   Rev 2.7   Jan 30 2006 14:23:58   t13ex43
      *AVB00827: PGM - SLM-/Fibu-Daten für DIM7-Aufbaubonus
      *
      *   Rev 2.6   Dec 28 2005 10:33:16   t13ex57
      *AVB00827: Neuauflage Riester-WB mit Neukundenausprägung
      *
      *   Rev 2.5   Jan 25 2005 09:30:36   t13ex57
      *AIS00057: zwei neue Leistungen in SLM
      *
      *   Rev 2.4   Jul 01 2003 09:51:48   t13ae35
      *AIS00053 - KNR für Wachstumsbonus aufgenommen
      *                                                                 
      *   Rev 2.3   Jan 02 2003 15:21:36   t13ae35                      
      *ASL00153 - 4 neue SLM-Typen                                      
      *                                                                 
      *   Rev 2.2   28 Sep 2000 14:50:10   T13AE30                      
      *ASL00067 - FOEBO2                                                
      *                                                                 
      *   Rev 2.1   Apr 25 2000 15:56:28   T13AE30                      
      *ASL00049 - Dimension 7 Aufbau-OLZ                                
      *                                                                 
      *   Rev 2.0   Feb 27 1999 11:57:18   P13SP20                      
      *NT-Umstellung                                                    
      *                                                                 
      *   Rev 1.1   Dec 19 1995 11:14:58   T13AE25                      
      *AFVB0244: Neue Tabelle KIRH                                      
      *                                                                 
      *   Rev 1.0   Nov 04 1995 08:51:58   P13SP99                      
      *Initial revision.                                                
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRAT0096   KNR-KONST. FUER SLM-TYP            * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: SCHMIDT                                          * 
      *    DATUM:    11.09.1995                                       * 
      ***************************************************************** 
      *    VERSION   1    02.10.95 15:15:05                             
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * --------+--------+----+---------------------------+----------*  
      * AFVB0226!11.09.95! 01 ! NEU                       ! SCHMIDT  *  
      * AFVB0226!18.12.95! 02 ! K-KNR-RUECKST-AUSZ-MAUNELL! SCHINDLER*  
      *         !        !    ! NEU                       !          *  
      * ASL00049!17.04.00! 03 ! K-KNR-FOERDERBONUS-DIM7   ! WILLIG   *  
      *         !        !    ! NEU                       !          *  
      * ASL00067!20.09.00! 04 ! K-KNR                     ! WILLIG   *  
      *         !        !    ! NEU                       !          *  
      * ASL00152!12.10.02! 2.3! 4 neue SLM-Typen          !J.Hering  *  
      * AIS00053!30.06.03! 2.4! KNR für LV-Wachstumsbonus !J.Hering  *  
170105* AIS00057!17.01.05!2.5 !Aufnahme Riester-Bonus,    ! U. Boll  *
      *         !        !    !Starter-Bonus Wunschpolice !          *
      * AVB00827!21.12.05!2.6 !Aufnahme Tankgutschein(TGS)!U. Boll   *
300106* AVB00827!30.01.06!2.7 ! DIM7-AUFBAUBONUS HINZU    ! KLEEMEIER*
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *   SCHLÜSSELWERTE FÜR SLM-TYP                                    
      *                                                                 
       01  FILLER.                                                      
           05  K-KNR-ERFOLGSPROVISION         PIC S9(004) COMP VALUE 01.
           05  K-KNR-ERFOLGSBONUS             PIC S9(004) COMP VALUE 02.
           05  K-KNR-BETREUUNGSBONUS          PIC S9(004) COMP VALUE 03.
           05  K-KNR-AUFBAUBONUS              PIC S9(004) COMP VALUE 04.
           05  K-KNR-ERFOLGSPROVISION-NEUE-AL PIC S9(004) COMP VALUE 05.
           05  K-KNR-AUFBAUBONUS-BLAU-GRUEN   PIC S9(004) COMP VALUE 06.
           05  K-KNR-AUFBAUZUSCHUSS-EINMALIG  PIC S9(004) COMP VALUE 07.
           05  K-KNR-AUFBAUZUSCHUSS-LAUFEND   PIC S9(004) COMP VALUE 08.
           05  K-KNR-FOERDERBONUS             PIC S9(004) COMP VALUE 09.
           05  K-KNR-ED-PROVISION-QUARTAL     PIC S9(004) COMP VALUE 10.
           05  K-KNR-UNTERSTUETZUNGSKASSE     PIC S9(004) COMP VALUE 11.
           05  K-KNR-RUECKST-AUSZ-MANUELL     PIC S9(004) COMP VALUE 12.
170400     05  K-KNR-FOERDERBONUS-DIM7        PIC S9(004) COMP VALUE 13.
200900     05  K-KNR-FOEBO2-DIM7              PIC S9(004) COMP VALUE 14.
101202     05  K-KNR-ERFOLGSPROVISION-NEU     PIC S9(004) COMP VALUE 15.
101202     05  K-KNR-AUFBAUBONUS-NEU          PIC S9(004) COMP VALUE 16.
101202     05  K-KNR-BETREUERBONUS            PIC S9(004) COMP VALUE 17.
101202     05  K-KNR-1000-EUR-GSH             PIC S9(004) COMP VALUE 18.
300603     05  K-KNR-LV-WTUMBONUS             PIC S9(004) COMP VALUE 19.
170105     05  K-KNR-RIESTERBONUS             PIC S9(004) COMP VALUE 20.
170105     05  K-KNR-WUNSCHPOLICE             PIC S9(004) COMP VALUE 21.
211205     05  K-KNR-TGS                      PIC S9(004) COMP VALUE 22.
300106     05  K-KNR-DIM7-AUFBAUBONUS         PIC S9(004) COMP VALUE 23.
      *                                                                 
      ******** ENDE COPY-MEMBER DRAT0096  KNR-K. SLM-TYP ************** 
