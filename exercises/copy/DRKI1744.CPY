      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.01.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1744.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1744.CPv  $
      *
      *   Rev 2.2   Oct 15 2007 18:12:22   t13ex48
      *AKI00677: Änderung der Zuordnung von unbetr. EDI-Dokumenten
      *
      *   Rev 2.1   Mar 31 2005 12:31:24   t13ex48
      *AKI00456: Projekt e-Post: Import und Prüfung der Metadaten d
      *er AML
      *
      *   Rev 2.0   Jan 12 2005 15:01:48   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1744 Funktion SELECT                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HSE                                             *
      *    DATUM:    10.01.2005                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00456!10.01.05! 01 ! NEUERSTELLUNG             ! HSE      *
      * -------------------------------------------------------------*
      * AKI00677!23.08.07! 02 ! Erweiterung PO-Bereich    ! HSE      *
      *         !        !    ! um ADMAPERSONENID         !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1744                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       JOZP VBDATEN ERMITTELN             *
      ****************************************************************
      *
       01 PI-DRKI1744.
          05 PI-DRKI1744-KEY.
             10 PI-DRKI1744-VRBVERTRAGSID        PIC X(13).
      *
       01 PO-DRKI1744.
          05 PO-DRKI1744-RC                      PIC  X(003).
             88 PO-DRKI1744-RC-OK                     VALUE 'OK '.
             88 PO-DRKI1744-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRKI1744-DATEN.
             10 PO-DRKI1744-MITARBEITERNR        PIC  9(007).
             10 PO-DRKI1744-DIR                  PIC  9(003).
             10 PO-DRKI1744-STUFE                PIC  9(003).
             10 PO-DRKI1744-AUSTR-DAT            PIC  X(010).
             10 PO-DRKI1744-KOMMBETR-KZ          PIC  X(001).
230807       10 PO-DRKI1744-ADMAPERSONENID       PIC  X(013).
      *
      ****** Ende Copy-Member ****************************************
