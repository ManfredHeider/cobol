      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0782.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0782.CPv  $
      *
      *   Rev 2.2   Apr 27 2015 15:50:18   t13ex13
      *ASL00530: RGT: SL05
      *
      *   Rev 2.1   Jul 11 2005 13:20:48   T13AE27
      *ASL00204: K-Leistungszuschuss
      *
      *   Rev 2.0   Jun 03 2005 09:28:42   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0782                                      *
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
      * ASL00530!21-04.15! 2.2! WACHSTUMSAGENDA           !GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0782                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0782.
          05 PI-DRVB0782-MA-ST-NR           PIC S9(009)      COMP.
          05 PI-DRVB0782-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB0782-ANSPR-GRUND        PIC S9(004)      BINARY.
      *
       01 PO-DRVB0782.
 
          05 PO-DRVB0782-RC                 PIC  X(003).
             88 PO-DRVB0782-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0782-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0782-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0782-RC-NOT-FOUND                 VALUE 'NFD'.
 
          05 PO-DRVB0782-DATEN.
210415       10 PO-DRVB0782-KLZA-TUPEL-GESAMT  PIC X(126).
      *
      ***** ENDE COPY-MEMBER DRVB0782 *****************************

