      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0784.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0784.CPv  $
      *
      *   Rev 2.1   Jul 11 2005 13:22:52   T13AE27
      *ASL00204: K-Leistungszuschuss
      *
      *   Rev 2.0   Jun 15 2005 11:28:20   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0784                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0046                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    01.05.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00204!01.06.05! 2.1! neu                       ! P.Duerr  *
      *         !        !    !                           !          *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0784                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0784.
          05 PI-DRVB0784-MA-ST-NR           PIC S9(009)      COMP.
          05 PI-DRVB0784-ANSPR-VOM          PIC  X(010).
      *
       01 PO-DRVB0784.

          05 PO-DRVB0784-RC                 PIC  X(003).
             88 PO-DRVB0784-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0784-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0784-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0784-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0784-DATEN.
             10 PO-DRVB0784-KLZA-VORHANDEN  PIC X(001).
      *
      ***** ENDE COPY-MEMBER DRVB0784 *****************************
