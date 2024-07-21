      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0568.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0568.CPv  $
      *
      *   Rev 2.4   Jun 22 2018 09:10:52   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.3   16 May 2007 06:43:42   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.2   Apr 13 1999 12:10:48   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B Korrektur
      *
      *   Rev 2.1   Apr 13 1999 10:41:40   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B
      *
      *   Rev 2.0   Apr 06 1999 15:42:02   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   22 Oct 1997 11:56:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0568                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<EE-PRUEFEN          >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    27.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.10.97! 01 ! NEU                       ! PRIGGEN  *
      *         !        !    !                           !          *
      * AFPA0012!19.06.98!    ! EINBAU EVS-NOTIZ          ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!        !2.1 ! CRPA0032                  !          *
      *         !30.03.99!    !  - 1.5.1.                 ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!        !2.1 ! CRPA0032                  !          *
      *         !13.04.99!    !  - 1.5.1. - KORREKTUR     ! WOLFF    *
      * APA00604!        !2.3 ! ABSCHALTUNG PEW           ! SCHEIDEL *
      * APA02579!21.06.18!2.4 ! HERKUNFT HINZU            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0568                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0568.
          05 PI-DRPA0568-DATEN.
             10 PI-DRPA0568-PMPE.
                COPY DRPA0289.
120407       10 PI-DRPA0568-ANTA-AVERM              PIC S9(009) BINARY.
      *   ACHTUNG: DIE REIHENFOLGE DER PVER MUSS ANALOG DER REIHENFOLGE
      *            DER VVNR IN PG-PM-DATEN SEIN; INCL. ANZAHL
             10 PI-DRPA0568-PVER-PVPT.
                   COPY DRPA0579.
      *   ACHTUNG: DIE REIHENFOLGE DER PMBE MUSS ANALOG DER REIHENFOLGE
      *            DER VVNR IN PG-PM-DATEN SEIN; INCL. ANZAHL
             10 PI-DRPA0568-PMBE-TABELLE.
                15 MAX-USED-PMBE                    PIC S9(004) BINARY.
                15 FILLER                           OCCURS 10.
300399             20 PMBE-KD-BETR                  PIC S9(009) BINARY.
210618       10 PI-DRPA0568-KNR-BEW-HRKFT           PIC S9(004) BINARY.
      *
       01 PO-DRPA0568.
          05 PO-DRPA0568-RC                         PIC  X(003).
             88 PO-DRPA0568-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0568-RC-NOK                        VALUE 'NOK'.
      *         (KEE = KEINE ERSATZ-/ERHOEHUNG)
             88 PO-DRPA0568-RC-KEE-KEIN-AVERM             VALUE 'KKA'.
             88 PO-DRPA0568-RC-KEE-KEIN-KDB               VALUE 'KKK'.
             88 PO-DRPA0568-RC-KEE-KDB-DIFF               VALUE 'KKD'.
190698    05 PO-DRPA0568-DATEN.
190698       10 PO-DRPA0568-EVS-AVERM               PIC S9(009) BINARY.
190698       10 PO-DRPA0568-KNR-EVS-ERM-AVERM       PIC S9(004) BINARY.
190698       10 PO-DRPA0568-TABELLE                 OCCURS 10
190698                                       INDEXED BY PO-DRPA0568-IX.
190698          15 PO-DRPA0568-KD-BETR              PIC S9(009) BINARY.
190698          15 PO-DRPA0568-KNR-EVS-ERM-KDBETR   PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

