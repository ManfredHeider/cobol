      * T13EX48   Stand: 16/11/98    16:53:28
      * T13EX31   Stand: 02/12/97    14:19:21
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0095.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0095.cpv  $
      *
      *   Rev 2.1   Jun 03 1999 12:42:48   T13EX48
      *FKI00027
      *Fehlerkorrekturen BA/RLK
      *
      *   Rev 2.0   Apr 16 1999 18:34:42   T13EX48
      *AKI00028
      *PI/PO Funktion DRKI0095
      *
      *   Rev 1.0   29 Sep 1997 11:44:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0095  FUNKTION LESEN RUECKLAUFKONTROLLE  *
      *                           (LIST-OZP: OPEN, FETCH, CLOSE)     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Lee                                             *
      *    DATUM:    30.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * KI      !30.12.97! 01 ! NEUERSTELLUNG             ! Lee      *
      * KI      !16.11.98! 02 ! Neues Feld:               ! HSE      *
      *         !        !    ! RUECKLAUFKONTRID          !          *
      * FKI27   !03.05.99! 03 ! Alle Felder aus TAB in den! HSE      *
      *         !        !    ! Uebergabebereich aufnehmen!          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI0095.
           05  PI-DRKI0095-UFKT-CODE          PIC  9(02).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI0095.
           05  PO-DRKI0095-RC                  PIC  X(03).
               88  PO-DRKI0095-RC-OK           VALUE 'OK '.
               88  PO-DRKI0095-RC-NOK          VALUE 'NOK'.
               88  PO-DRKI0095-RC-NFD          VALUE 'NFD'.
               88  PO-DRKI0095-RC-LEER         VALUE 'LER'.
               88  PO-DRKI0095-RC-FETCH-ENDE   VALUE 'EOF'.
           05  PO-DRKI0095-DATEN.
               10  PO-DRKI0095-RUECKLAUFKONTRID PIC X(13).
               10  PO-DRKI0095-GES              PIC 99.
               10  PO-DRKI0095-VSNR             PIC 9(015).
               10  PO-DRKI0095-ABSENDEDAT       PIC X(010).
               10  PO-DRKI0095-GES-KZ           PIC 99.
990503         10  PO-DRKI0095-KUNDENNUMMER     PIC 9(07).
990503         10  PO-DRKI0095-VBNUMMER         PIC 9(07).
               10  PO-DRKI0095-BESCHR           PIC X.
990503         10  PO-DRKI0095-ERLEDIGUNGSKZ    PIC 99.
               10  PO-DRKI0095-ERLEDIGT-DAT     PIC X(10).
990503         10  PO-DRKI0095-KUNDENNAME       PIC X(30).
990503         10  PO-DRKI0095-BESUCHSAUFTRART  PIC X.
               10  PO-DRKI0095-PCVWPC           PIC 9(07).
990503         10  PO-DRKI0095-PCVBPC           PIC 9(07).
      *
      ******** ENDE COPY-MEMBER (DRKI0095) *******************

