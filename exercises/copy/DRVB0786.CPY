      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0786.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0786.CPv  $
      *
      *   Rev 2.1   Jul 05 2005 14:07:28   t13ex43
      *ASL00206: PGM 2 - Familien-Absicherungs-Plan
      *
      *   Rev 2.0   Jun 27 2005 10:03:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0786                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0027                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.06.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00206!27.06.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0786                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0786.
          05 PI-DRVB0786-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0786-FAPZ-DAT-VON       PIC  X(010).
      *
       01 PO-DRVB0786.
          05 PO-DRVB0786-RC                 PIC  X(003).
             88 PO-DRVB0786-RC-OK                VALUE 'OK '.
             88 PO-DRVB0786-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0786-DATEN.
             10 PO-DRVB0786-FAPZ-TUPEL-GESAMT    PIC  X(070).
      *
      ***** ENDE COPY-MEMBER DRVB0786 *****************************
      *
