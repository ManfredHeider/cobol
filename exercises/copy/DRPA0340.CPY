      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0340.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0340.cpv  $
      *
      *   Rev 2.1   Mar 31 2000 12:41:52   T13AE34
      *APA00081 - PEW DAMO mit PGEB
      *
      *   Rev 2.0   Feb 27 1999 14:28:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   20 Oct 1997 17:25:40   T13EX22
      *AFPA0018 - Ein- und Ausgabeparameter fuer DRPA0340
      *
      *   Rev 1.0   01 Jul 1997 16:20:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0340  LIST PEWA                          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. MUND                                         *
      *    DATUM:    04.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0018!04.08.97! 01 ! Neuerstellung             ! T. MUND  *
      * APA00081!30.03.00!2.1 ! PEW DAMO MIT PGEB         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0340                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST  <<<   TABELLE PEWA TBPA0011                    *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0340.
          05 PI-DRPA0340-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRPA0340.
          05 PO-DRPA0340-RC                 PIC  X(003).
             88 PO-DRPA0340-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0340-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0340-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0340-DATEN.
             10 PO-DRPA0340-PEWA-ID           PIC S9(15)V USAGE COMP-3.
300300       10 PO-DRPA0340-PGEB-GES          PIC S9(04)  USAGE COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
