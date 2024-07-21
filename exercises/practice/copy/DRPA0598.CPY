      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0598.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRPA0598.CPv  $
      *
      *   Rev 2.14   16 May 2007 06:43:58   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.13   Nov 30 2001 09:08:40   t13ex43
      *APA00167 : DRPA1514 hinzu (PIN prüfen Sonst. Banken Österreich)
      *
      *   Rev 2.12   Nov 28 2001 16:34:06   t13ex22
      *APA00167: Ergänzungen für DIT-Österreich
      *
      *   Rev 2.11   05 Apr 2001 16:17:08   T13EX43
      *APA00130 : DRPA1488 hinzu
      *
      *   Rev 2.10   13 Mar 2001 16:29:56   T13EX25
      *APA00131; PEG Central
      *
      *   Rev 2.9   07 Feb 2001 14:03:52   T13EX22
      *APA00118: Neue Funktion Pin prüfen für BfG
      *
      *   Rev 2.8   19 Dec 2000 14:58:10   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.7   02 Nov 2000 13:31:40   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen
      *
      *   Rev 2.6   15 Sep 2000 09:27:00   T13EX21
      *APA00098 - Neue Funktion
      *
      *   Rev 2.5   09 Aug 2000 08:21:40   T13EX22
      *APA00098: Neue Funktion: PIN-Prüfung für iii
      *
      *   Rev 2.4   22 May 2000 16:06:22   T13EX44
      *Neue Funktion DRPA1384
      *
      *   Rev 2.3   May 04 2000 11:50:10   T13EX23
      *APA00067 - Neue Funktion DRPA1372
      *
      *   Rev 2.2   Apr 13 2000 14:55:58   T13EX21
      *APA00067 - Neue Funktionen
      *
      *   Rev 2.1   05 Nov 1999 13:58:40   T01JM2S
      *APA00065 - VERTRAGSSTRUKTUR VON EVS-PRODUKTEN VERDICHTEN
      *
      *   Rev 2.0   May 27 1999 12:32:50   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   22 Oct 1997 13:13:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0598  FUNKTION                           *
      *                           "VERTRAG ERMITTELN"                *
      *                 DRPA0604  "PVER-DATEN ERMITTELN"             *
      *                 DRPA0627  "PVER- UND PVPT-DATEN ERMITTELN"   *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    11.11.1997                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !11.11.97!01.01! NEUERSTELLUNG             ! JÄGER   *
      *         !        !     !                           !         *
      * APA00065!02.11.99! 2.1 ! EINFÜGEN FUNKTION DRPA0744! WOLFF   *
      *         !        !     !                           !         *
      * APA00067!29.03.00! 2.2 ! NEUE FUNKTIONEN           ! STORCH  *
      *         !        !     !                           !         *
      * APA00067!18.04.00! 2.3 ! NEUE FUNKTION DRPA1372    ! GRAY    *
      *         !        !     !                           !         *
      * APA00090!12.05.00! 2.4 ! NEUE FUNKTION DRPA1384    ! SCHEIDEL*
      * APA00098!07.08.00! 2.5 ! NEUE FUNKTION DRPA1415    ! MUND    *
      * APA00098!07.09.00! 2.6 ! NEUE FUNKTION DRPA1421    ! STORCH  *
      * APA00112!25.10.00! 2.7 ! NEUE FUNKTION DRPA1428    !KLEEMEIER*
      * APA00118!07.02.01! 2.8 ! NEUE FUNKTION DRPA1467    !MUND     *
      * APA00131!23.02.01! 2.9 ! NEUE FUNKTION DRPA1474    ! Jäger   *
      * APA00131!05.04.01! 2.11! NEUE FUNKTION DRPA1488    !KLEEMEIER*
      * APA00167!22.11.01! 2.12! NEUE FUNKTION DRPA1511    !MUND     *
      * APA00167!28.11.01! 2.13! NEUE FUNKTION DRPA1514    !KLEEMEIER*
      * APA00604!12.04.07! 2.14! NEUE FUNKTION DRPA1662    ! SCHEIDEL*
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0077.
           05 PI-UPPA0077-FUNKTION            PIC X(008).
              88 PI-UPPA0077-DRPA0604             VALUE 'DRPA0604'.
              88 PI-UPPA0077-DRPA0627             VALUE 'DRPA0627'.
021199        88 PI-UPPA0077-DRPA0744             VALUE 'DRPA0744'.
290300        88 PI-UPPA0077-DRPA1361             VALUE 'DRPA1361'.
290300        88 PI-UPPA0077-DRPA1362             VALUE 'DRPA1362'.
290300        88 PI-UPPA0077-DRPA1363             VALUE 'DRPA1363'.
290300        88 PI-UPPA0077-DRPA1371             VALUE 'DRPA1371'.
180400        88 PI-UPPA0077-DRPA1372             VALUE 'DRPA1372'.
120500        88 PI-UPPA0077-DRPA1384             VALUE 'DRPA1384'.
070800        88 PI-UPPA0077-DRPA1415             VALUE 'DRPA1415'.
070900        88 PI-UPPA0077-DRPA1421             VALUE 'DRPA1421'.
251000        88 PI-UPPA0077-DRPA1428             VALUE 'DRPA1428'.
070201        88 PI-UPPA0077-DRPA1467             VALUE 'DRPA1467'.
230201        88 PI-UPPA0077-DRPA1474             VALUE 'DRPA1474'.
050401        88 PI-UPPA0077-DRPA1488             VALUE 'DRPA1488'.
221101        88 PI-UPPA0077-DRPA1511             VALUE 'DRPA1511'.
281101        88 PI-UPPA0077-DRPA1514             VALUE 'DRPA1514'.
120407        88 PI-UPPA0077-DRPA1662             VALUE 'DRPA1662'.
      *
