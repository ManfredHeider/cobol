      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0108.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRAT0108.CPv  $
      *
      *   Rev 2.2   12 Sep 2001 14:33:38   t13ae46
      *APA00144: Neu: K-INSERT-SICHERUNG
      *
      *   Rev 2.1   Mar 11 1999 16:12:44   T13EX49
      *AKI00018 - 11.03.1999
      *
      *   Rev 2.0   Mar 10 1999 12:03:40   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.3   13 Nov 1998 09:07:04   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.2   06 Aug 1998 10:19:50   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   20 Oct 1997 17:32:42   T13EX22
      *AFPA0018 - Konstanten fuer Unterfunktionscodes
      *
      *   Rev 1.0   May 26 1997 15:21:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: KONSTANTEN FUER UNTERFUNKTINSCODES           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    13.06.1996                                      *
      ****************************************************************
      *    VERSION   0
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!05.10.96! 01 ! UFKT FüR DAS BUFFER-OZP   ! AUERBACH *
      *         !        !    !                           !          *
100697* AFPA0012!10.06.97! 02 ! UFKT FÜR DAS STANDARD-OZP ! WOLFF    *
100697*         !        !    ! (INSERT)                  !          *
      * AFPA0012!11.01.98! 03 ! UFKT FüR DAS STANDARD-OZP ! STORCH   *
      *         !        !    ! (UPDATE)                  !          *
      * APA00012!02.02.98! 03 ! UFKT FüR DAS STANDARD-OZP ! BOHLINGER*
      *         !        !    ! (INSERT-DATEI-FIRST UND   !          *
      *         !        !    !INSERT-DATEI-NEXT) ENTFERNT!          *
      *         !        !    !                           !          *
      *         !10.02.98! 04 ! UFKT FüR DAS SERVICE-MODUL! BOHLINGER*
      *         !        !    ! (WRITE-)                  !          *
      * APA00020!21.02.98! 05 ! UFKT FüR DAS buffer-ozp   ! BOHLINGER*
      *         !        !    ! (put-...)                 !          *
      *         !        !    !                           !          *
      * AKI00001!19.06.98! 06 ! UFKT K-WRITE-QUIT ERGÄNZT ! SCHÄFER  *
      *         !        !    !                           !          *
      * APA00012!15.07.98! 07 ! UFKT K-DELETE-... ERGÄNZT ! Jäger    *
      * APA00020!24.08.98! 05 !buffer-ozp:                ! BOHLINGER*
      *         !        !    !ausprägungen für ergebnis- !          *
      *         !        !    !zeilentyp und komprimierung!          *
      *         !        !    !                           !          *
      * AFPA0012!07.09.98!    ! NEU: K-LIST-RESTART       ! WOLFF    *
      * APA00012!14.11.98! 08 ! NEU: K-UPDATE-PNAP        ! MUND     *
      * APA00144!09.08.01! 09 ! NEU: K-INSERT-SICHERUNG   ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   UNTERFUNKTIONSCODE (UFKT) FüR DAS LIST-OZP
       01  FILLER.
           05  K-LIST-FIRST                   PIC  99    VALUE 01.
           05  K-LIST-NEXT                    PIC  99    VALUE 02.
           05  K-LIST-QUIT                    PIC  99    VALUE 03.
070998     05  K-LIST-RESTART                 PIC  99    VALUE 04.
      *
      *   UNTERFUNKTIONSCODE (UFKT) FüR DAS BUFFER-OZP
           05  K-GET-ALL                      PIC  99    VALUE 01.
           05  K-GET-FIRST                    PIC  99    VALUE 02.
           05  K-GET-NEXT                     PIC  99    VALUE 03.
           05  K-GET-QUIT                     PIC  99    VALUE 04.

      *   UNTERFUNKTIONSCODE (UFKT) FüR DAS BUFFER-OZP
210298     05  K-PUT-FIRST                    PIC  99    VALUE 05.
210298     05  K-PUT-NEXT                     PIC  99    VALUE 06.
210298     05  K-PUT-UPD                      PIC  99    VALUE 07.
      *
      *   AUSPRäGUNGEN DES ERGEBNISZEILEN-TYPS FüR DAS BUFFER-OZP
240898     05  K-ZEILEN-TYP-HAUPTEINTRAG      PIC  99    VALUE 01.
240898     05  K-ZEILEN-TYP-UNTEREINTRAG      PIC  99    VALUE 02.

      *   AUSPRäGUNGEN ZUR STEUERUNG DER KOMPRIMIERUNG DES BUFFER-OZP
      *        HAUPEINTRAG UND NACHFOLGENDE UNTEREINTRäGE üBERNEHMEN
240898     05  K-STRG-KOMP-UNTEREINTRAG-NEIN  PIC  99    VALUE 01.
      *        HAUPEINTRAG üBERNEHMEN, NACHFOLGENDE UNTEREINTRäGE KOMP.
240898     05  K-STRG-KOMP-UNTEREINTRAG-JA    PIC  99    VALUE 02.
      *
      *
100697*
100697*   UNTERFUNKTIONSCODE (UFKT) FÜR DAS STANDARD-OZP (INSERT)
100697     05  K-INSERT-DB2                   PIC  99    VALUE 01.
100697     05  K-INSERT-DATEI                 PIC  99    VALUE 02.
060298*    05  K-INSERT-DATEI-FIRST           PIC  99    VALUE 02.
060298*    05  K-INSERT-DATEI-NEXT            PIC  99    VALUE 03.
100697     05  K-INSERT-DATEI-QUIT            PIC  99    VALUE 04.
090801     05  K-INSERT-SICHERUNG             PIC  99    VALUE 05.

150798*   UNTERFUNKTIONSCODE (UFKT) FÜR DAS STANDARD-OZP (DELETE)
150798     05  K-DELETE-DB2                   PIC  99    VALUE 09.
150798     05  K-DELETE-DATEI                 PIC  99    VALUE 10.
150798     05  K-DELETE-DATEI-QUIT            PIC  99    VALUE 11.

110198*   UNTERFUNKTIONSCODE (UFKT) FÜR DAS STANDARD-OZP (UPDATE)
110198*   OVW: OHNE VERSIONSWECHSEL
110198*   MVW: MIT VERSIONSWECHSEL
141198*   PNAP: UPDATE PVER-PNAP-EINTRAG
110198     05  K-UPDATE-OVW                   PIC  99    VALUE 05.
110198     05  K-UPDATE-MVW                   PIC  99    VALUE 06.
141198     05  K-UPDATE-PNAP-EINTRAG          PIC  99    VALUE 07.

110198*   UNTERFUNKTIONSCODE (UFKT) FÜR DAS STANDARD-OZP (SELECT)
110198*   AKT: AKTUELLSTEN SATZ
110198*   VER: MIT VERSION
110198     05  K-SELECT-AKT                   PIC  99    VALUE 07.
110198     05  K-SELECT-VER                   PIC  99    VALUE 08.

100298*   UNTERFUNKTIONSCODE (UFKT) FÜR DAS SERVICE-MODULE(WRITE)
100298     05  K-WRITE-FIRST                  PIC  99    VALUE 01.
100298     05  K-WRITE-NEXT                   PIC  99    VALUE 02.
190698     05  K-WRITE-QUIT                   PIC  99    VALUE 03.
      *
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRAT0108) ********
