      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0086.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0086.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   Sep 27 1996 16:01:28   T13AE26
      *AFVB0309: AL-D mehrfach; neuer Returncode
      *          PO-VB71-RC-ZUSA-NFACH-OK
      *
      *   Rev 1.2   Jun 19 1996 11:02:42   T13AE28
      *Neue Funktion "PI-VB71-FKT-ZUSA-PRUEF-VB11" zur Prüfung der zulässigen
      *Zusätze zur Stufe AL bei Neuaufnahme aufgenommen
      *
      *   Rev 1.1   Feb 15 1996 09:23:50   T13AE26
      *AFVB0251: Änderung wegen neuer Funktion von UPVB0071
      *          'VB71-FKT-HST-ZUSA-ERMITT'
      *
      *   Rev 1.0   Dec 29 1995 15:42:02   T13AE26
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0086                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE UPVB0071  Ermitteln Zusatz zur MA-Stufe      *
      *                                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Moll                                             *
      *    DATUM:    30.11.1995                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * AFVB0251!10.12.95!    ! NEU                       ! Moll      *
      * AFVB0251!05.01.96!    ! Neue Funktion             !           *
      *         !        ! 01 ! -VB71-FKT-HST-ZUSA-ERMITT ! Moll      *
      * AFVB0285!03.06.96!    ! Neue Funktion             !           *
      *         !        !    ! ..-ZUSA-PRUEF-VB11        ! Ehrmann   *
      * AFVB0309!24.09.96!    ! Zusatz 'D' auch n-fach    !           *
      *         !        !    ! möglich                   ! Moll      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *    VERSION
      ****************************************************************
      *
      *    FUNKTION DES UPVB0071:
      *
      *     Prüfen/Setzen/Beenden von Zusätzen zur MA-Stufe
      *     (z.Z. AL-Probe/-'K'nade/-Dauer).
      *
      *     Diese Zusätze müssen eindeutig sein, d.h. es kann
      *     jeweils nur ein gültiger Zusatz gesetzt sein.
      *     'Gültig' heißt zur Zeit (12/95), daß MAST_DAT_BIS
      *     auf K-DAT-BIS-MAX-ISO gesetzt ist.
      *
      *     Die möglichen Zusätze werden durch die Elemente
      *     einer Gruppe von 'MAST-Zusätzen' (KNR_GR = 001)
      *     definiert.
      *
      *     Das Upro prüft keine Abhängigkeiten von der Stufe,
      *     die der ST-NR zugeordnet ist.
      *
      *     Diese Abhängigkeit ist zur Zeit (12/95) in einer
      *     Working-Storage Tabelle (DRVB0085) abgelegt.
      *
      *
      *    EINGABE:
      *
      *      Immer:
      *        - PI-UPVB0071-FKT
      *        - PI-UPVB0071-ST-NR
      *        - PI-UPVB0071-KNR-GR
      *      Falls Zusatz prüfen:
      *        - PI-UPVB0071-ZUSA
      *      Falls gült. Zusatz zu Zeitpunkt ermitteln:
      *        - PI-UPVB0071-ZTPKT-EURO
      *      Falls Zusatz setzen:
      *        - PI-UPVB0071-ZUSA
      *        - PI-UPVB0071-DATVON-TIMESTAMP
      *        - PI-UPVB0071-STD-DATEN
      *      Falls Zusatz beenden:
      *        - PI-UPVB0071-DATBIS-ISO
      *        - PI-UPVB0071-STD-DATEN
      *        - PI-UPVB0071-LAST-UPDATE
      *
      *    AUSGABE:
      *
      *      Immer:
      *        - PO-UPVB0071-RC
      *      Beim Ermitteln:
      *        - PO-UPVB0071-ZUSA
      *        - PO-UPVB0071-DATVON-EURO
      *        - PO-UPVB0071-LAST-UPDATE
      *      Beim Setzen oder Beenden:
      *        - PO-UPVB0071-PARAMETER-RETURN
      *
       01  PI-UPVB0071.
           05  PI-UPVB0071-FKT                  PIC   9(002).
               88  PI-VB71-FKT-AKT-ZUSA-ERMITT  VALUE 1.
               88  PI-VB71-FKT-ZUSA-PRUEF       VALUE 2.
               88  PI-VB71-FKT-ZUSA-SETZ        VALUE 3.
               88  PI-VB71-FKT-AKT-ZUSA-BEEND   VALUE 4.
               88  PI-VB71-FKT-HST-ZUSA-ERMITT  VALUE 5.
030696         88  PI-VB71-FKT-ZUSA-PRUEF-VB11  VALUE 6.

           05  PI-UPVB0071-ST-NR                PIC  S9(09) COMP.
           05  PI-UPVB0071-KNR-GR               PIC  S9(04) COMP.
           05  PI-UPVB0071-ZUSA                 PIC  X(01).
           05  PI-UPVB0071-DATVON-TIMESTAMP     PIC  X(26).
           05  PI-UPVB0071-DATBIS-ISO           PIC  X(10).
           05  PI-UPVB0071-LAST-UPDATE          PIC  X(26).
           05  PI-UPVB0071-ZTPKT-EURO           PIC  X(10).

           05  PI-UPVB0071-STD-DATEN.
               10 PI-UPVB0071-PROGRAMM-ID       PIC  X(08).
               10 PI-UPVB0071-PROGRAMM-KZ       PIC  X(01).
               10 PI-UPVB0071-USER-ID           PIC  X(07).
               10 PI-UPVB0071-TERMINAL-ID       PIC  X(04).

           05  FILLER                           PIC  X(16).
      *
       01  PO-UPVB0071.
           05  PO-UPVB0071-RC                   PIC   9(002).
               88  PO-VB71-RC-OK                VALUE 0.
               88  PO-VB71-RC-AKT-ZUSA-NFD      VALUE 1.
               88  PO-VB71-RC-ZUSA-UNGUELTIG    VALUE 2.
               88  PO-VB71-RC-ZUSA-GABS-SCHON   VALUE 3.
               88  PO-VB71-RC-KONKUR-ZUGRIFF    VALUE 4.
240996         88  PO-VB71-RC-ZUSA-NFACH-OK     VALUE 5.

           05  PO-UPVB0071-ZUSA                 PIC  X(01).
           05  PO-UPVB0071-DATVON-EURO          PIC  X(10).
           05  PO-UPVB0071-LAST-UPDATE          PIC  X(26).

      *          zum Durchreichen der Meldungen des
      *          Schreibmoduls an das HPro...
           05  PO-UPVB0071-PARAMETER-RETURN     PIC  X(82).

           05  FILLER                           PIC  X(16).

      ******** ENDE COPY-MEMBER (DRVB0086) *******************
