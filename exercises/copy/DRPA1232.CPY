      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1232.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1232.CPv  $
      *
      *   Rev 2.2   16 Jul 1999 11:05:46   T01JM2E
      *APA00012 - AVERM AUF 9(10) geändert
      *
      *   Rev 2.1   May 25 1999 15:39:04   T01JM2E
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:08   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1232                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  ARBEITSSATZPAKET ERMITTELN               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    11.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.05.99! 01 ! NEU                       !D.WILHELM *
      *         !        !    !                           !          *
      * AFPA0012!08.06.99! 2.1! AVERM AUF 9(10) Ändern    !D.WILHELM *          
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1232                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1232.
          05 PI-DRPA1232-UFKT-CODE               PIC  9(002).
          05 PI-DRPA1232-PROGRAMM                PIC  X(008).
          05 PI-DRPA1232-BUCHDAT                 PIC  9(008).                   
          05 PI-DRPA1232-DATEN.
             COPY DRPA1070.                                                     
      *
       01 PO-DRPA1232.
          05 PO-DRPA1232-RC                      PIC  X(003).
             88 PO-DRPA1232-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1232-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRPA1232-RC-EOF                    VALUE 'EOF'.
          05 PO-DRPA1232-DATEN.
             10 PO-DRPA1232-GES               PIC S9(004)  USAGE COMP.          
             10 PO-DRPA1232-VNR               PIC  X(015).                      
             10 PO-DRPA1232-KUNDE-NAME        PIC  X(025).                      
             10 PO-DRPA1232-PROD-PROD-SCHL    PIC S9(004)  USAGE COMP.          
             10 PO-DRPA1232-PDPT-PRODART      PIC S9(004)  USAGE COMP.          
             10 PO-DRPA1232-PMPT-PPFL-BEITRAG PIC S9(009)V9(2)
                                                         USAGE COMP-3.
080699       10 PO-DRPA1232-PEST-AVERM        PIC  9(010).                      
             10 PO-DRPA1232-EINH              PIC S9(6)V99
                                                       PACKED-DECIMAL.
             10 PO-DRPA1232-BUCHDAT           PIC  9(8).                        
      *
      ******* ENDE COPY-MEMBER DRPA1232 *****************************
