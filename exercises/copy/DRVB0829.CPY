      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0829.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0829.CPv  $
      *
      *   Rev 2.1   31 Jul 2006 15:37:48   t13ex13
      *FSL00184: Korrektur Versicherungssummen: RGT
      *
      *   Rev 2.0   25 Jul 2006 10:32:12   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0829                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0027                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    AUSGABE DES AKTUELLSTEN FAP-SATZES FÜR EIN VB              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    25.07.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * FSL00184!25.07.06!2.0 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0829                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0829.
          05 PI-DRVB0829-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB0829.
          05 PO-DRVB0829-RC                 PIC  X(003).
             88 PO-DRVB0829-RC-OK                VALUE 'OK '.
             88 PO-DRVB0829-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0829-DATEN.
             10 PO-DRVB0829-FAPZ-TUPEL-GESAMT    PIC  X(070).
      *
      ***** ENDE COPY-MEMBER DRVB0829 *****************************
      *
