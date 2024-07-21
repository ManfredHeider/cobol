      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0415.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0415.CPv  $
      *
      *   Rev 2.1   Jul 26 2006 08:41:18   t13ex43
      *ACV00054: PGM - CTV-Briefe mit CICS-Datum
      *
      *   Rev 2.0   Jul 19 2006 10:40:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0415                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0006                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.07.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ACV00054!19.07.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRAT0415                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRAT0415.
          05 PI-DRAT0415-AUFRUF-TYP         PIC  X(001).
      *
       01 PO-DRAT0415.
          05 PO-DRAT0415-RC                 PIC  X(003).
             88 PO-DRAT0415-RC-OK                VALUE 'OK '.
             88 PO-DRAT0415-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRAT0415-DATEN.
             10 PO-DRAT0415-CICS-DATUM      PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRAT0415 *****************************
      *
