      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0543.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0543.CPv  $
      *
      *   Rev 2.2   Jun 29 1999 13:50:48   T13EX21
      *APA00020 - Erweiterung PO-Bereich
      *
      *
      *   Rev 2.1   May 27 1999 09:44:00   T13EX25
      *APA00042, Korr. V2.0
      *
      *   Rev 2.0   May 27 1999 08:58:52   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   01 Oct 1997 11:28:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0543                                      
      *    --------------------------------------------------------   
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0543                    
      *    --------------------------------------------------------   
      *    ERSTELLT: BOHLINGER                                        
      *    DATUM:    21.10.1997                                       
      ****************************************************************
      *    VERSION                                                    
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97! 01 ! NEU                       ! BOHLINGER*
      * APA00012!22.06.99!    ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                               
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          
      *    FÜR DIE FUNKTION DRPA0543                                  
      *    AUSSER: DEM FUNKTIONSNAMEN,                                
      *            DEM ERR-ERROR-BEREICH UND                          
      *            DEM STANDARD-STATUS-BEREICH                        
      *            (JEWEILS SEPARATE COPYS)                           
      *                                                               
      *    FUNKTION DRPA0543 DES BUFFER-OZP ZYPA0010 ERMITTELT        
      *    DIE ERGEBNISMENGE FüR SF42, "üBERSICHT PRODUKTGRUPPE".     
      *                                                               
      ****************************************************************
      *                                                               
       01  PI-DRPA0543.                                               
           05  PI-DRPA0543-UFKT                 PIC  9(02).           
           05  PI-DRPA0543-NAME-ZW-SPEICHER     PIC  X(08).           
           05  PI-DRPA0543-LOG-SATZ-NR          PIC  9(05).           
           05  PI-DRPA0543-AUSWAHL-KRITERIEN.                         
               10 PI-DRPA0543-PGEB-GES          PIC S9(04) COMP.      
               10 PI-DRPA0543-PROD-PROD-SCHL    PIC S9(04) COMP.      
               10 PI-DRPA0543-S-PROD-SCHL       PIC  X(01).           
               10 PI-DRPA0543-STICHTAG          PIC  X(10).           
               10 PI-DRPA0543-S-STICHTAG        PIC  X(01).           
      *                                                               
       01  PO-DRPA0543.                                               
           05  PO-DRPA0543-RC                   PIC  X(03).            
               88 PO-DRPA0543-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0543-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0543-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0543-ANZAHL-ERG-ZEILEN    PIC  9(05).            
                                                                       
           05  PO-DRPA0543-ERGEBNISZEILE.                              
               10 PO-DRPA0543-PGRP-ID           PIC S9(004) COMP.      
220699         10 PO-DRPA0543-KNR-PGRP-ART      PIC S9(004) COMP.
               10 PO-DRPA0543-PPGR-GUELT-AB     PIC  X(010).           
               10 PO-DRPA0543-PPGR-GUELT-BIS    PIC  X(010).           
               10 PO-DRPA0543-PGRP-ART-BL       PIC  X(080).           
               10 PO-DRPA0543-PGRP-ART-AUSPR-BM PIC  X(010).           
      *                                                                
      ******* ENDE COPY-MEMBER **************************************  