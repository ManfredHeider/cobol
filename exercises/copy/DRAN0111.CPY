      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0111.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0111.CPv  $
      *
      *   Rev 2.4   Dec 18 2015 08:58:36   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.3   23 Nov 2007 11:02:16   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.2   01 Sep 2006 09:49:28   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Jan 17 2006 13:57:06   t13ae23
      *AAN00633: Automatisierung Testdatenbereitstellung Advocard D
      *FÜ-Sätze
      *
      *   Rev 2.0   Jan 09 2006 15:08:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0111  FUNKTION LIST-OZP FÜR TBAN0005 ADVC*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00633!17.01.06! 2.1! NEUERSTELLUNG             ! RAHNER   *
      * AAN00730!30.11.07! 2.2! ALLFINANZ                 ! RAHNER   *
      * AAN01223!08.12.15! 2.3! C+L WG. DRAN0083          ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0111                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0111.
           05 PI-DRAN0111-PROGRAMM             PIC  X(008).
           05 PI-DRAN0111-UFKT-CODE            PIC  9(002).
      *
        01 PO-DRAN0111.
           05 PO-DRAN0111-RC                   PIC  X(003).
             88 PO-DRAN0111-RC-OK              VALUE 'OK '.
             88 PO-DRAN0111-RC-LEER            VALUE 'LER'.
             88 PO-DRAN0111-RC-EOF             VALUE 'EOF'.
      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0083.
      *
      ***** ENDE-COPY-MEMBER *****************************************

