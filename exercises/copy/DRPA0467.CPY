      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0467.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0467.CPv  $
      *
      *   Rev 2.2   25 May 2000 10:12:08   T13EX23
      *APA00090 - Feldname aendern
      *
      *   Rev 2.1   22 May 2000 10:32:04   T13EX23
      *APA00090 - PINZ-DATEN einfügen 	
      *
      *   Rev 2.0   Mar 10 1999 12:10:48   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:40:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0467                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<VERTRAG VERGLEICHEN >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    21.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!        ! 01 ! NEU                       ! WOLFF    *
      *         !12.11.97!    ! ÄNDERUNG RC'S             ! MUND     *
      *         !        !    !                           !          *
      * AFPA0012!03.12.97!    ! ÄNDERUNGSKENNZEICHEN      ! WOLFF    *
      *         !        !    ! RAUSGENOMMEN              !          *
      * APA00090!18.05.00! 2.1! PI / PO BEREICHE FUER PINZ! GRAY     *
      *         !        !    ! HINZUFUEGEN               !          *
      * APA00090!25.05.00! 2.2! FELDNAME PNR-VNR AKZ AUF  ! GRAY     *
      *         !        !    ! PIN-VNR-AKZ AENDERN       !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0467                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0467.
          05 PI-DRPA0467-DATEN.
             10 PI-DRPA0467-PMPE.
                COPY DRPA0289.
             10 PI-DRPA0467-PVER.
                COPY DRPA0408.
180500       10 PI-DRPA0467-PINZ.
180500          COPY DRPA1385.
250500       10 PI-DRPA0467-PIN-VNR-AKZ             PIC S9(004) BINARY.
      *
       01 PO-DRPA0467.
          05 PO-DRPA0467-RC                         PIC  X(003).
             88 PO-DRPA0467-RC-OK                   VALUE 'OK '.
031297*   05 PO-DRPA0467-AKZ                        PIC  X(003).
031297*      88 PO-DRPA0467-KZ-AEND-MIT-HIST        VALUE 'AMH'.
031297*      88 PO-DRPA0467-KZ-AEND-OHNE-HIST       VALUE 'AOH'.
031297*      88 PO-DRPA0467-KZ-KEINE-AENDERUNG      VALUE 'KAE'.
          05 PO-DRPA0467-DATEN.
             10 PO-DRPA0467-PVER.
                COPY DRPA0408.
             10 PO-DRPA0467-VNR-ALT                 PIC  X(015).
180500       10 PO-DRPA0467-PINZ.
180500          COPY DRPA1385.
      ******* ENDE COPY-MEMBER **************************************
