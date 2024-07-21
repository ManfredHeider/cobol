      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0779.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0779.CPv  $
      *
      *   Rev 2.3   Apr 27 2015 15:50:14   t13ex13
      *ASL00530: RGT: SL05
      *
      *   Rev 2.2   Mar 20 2008 11:42:04   t13ex43
      *ASL00298: PGM - Änderungdialog SL05 für KLZ
      *
      *   Rev 2.1   Jul 11 2005 13:16:40   T13AE27
      *ASL00204: K-Leistungszuschuss
      *
      *   Rev 2.0   May 31 2005 12:18:50   t13ae27
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0779                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0082                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      *    ERSTELLT: P.Duerr                                         *
      *    DATUM:    14.10.2003                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00172!14.10.03! 2.1! neu                       ! P.Duerr  *
120308* ASL00298!12.03.08! 2.2! ANPASSUNG AN SL05         ! KLEEMEIER*
210415* ASL00530!21.04.15! 2.3! WACHSTUMSAGENDA           !GUTFLEISCH*
      *                                                              *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0779                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB0779.
          05 PI-DRVB0779-UFKT-CODE          PIC  9(002).
120308    05 PI-DRVB0779-MA-ST-NR           PIC S9(009) BINARY.
120308    05 PI-DRVB0779-KLZA-ANSPR-VON     PIC  X(010).
120308    05 PI-DRVB0779-KLZA-ANSPR-BIS     PIC  X(010).
          05 PI-DRVB0779-ANSPR-STATUS       PIC S9(004) BINARY.
      *
       01 PO-DRVB0779.
          05 PO-DRVB0779-RC                 PIC  X(003).
             88 PO-DRVB0779-RC-OK                VALUE 'OK '.
             88 PO-DRVB0779-RC-LEER              VALUE 'LER'.
             88 PO-DRVB0779-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB0779-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0779-DATEN.
             10 PO-DRVB0779-KLZA-MA-ST-NR      PIC S9(009)      BINARY.
             10 PO-DRVB0779-KLZA-ANSPR-VOM     PIC X(010).
             10 PO-DRVB0779-KNR-ANSPR-GRUND    PIC S9(004)      BINARY.
120308       10 PO-DRVB0779-KTY-ANSPR-GRUND    PIC S9(004)      BINARY.
120308       10 PO-DRVB0779-KLZA-LAST-UPDATE   PIC  X(026).
120308       10 PO-DRVB0779-KLZA-BEITRAG       PIC S9(09)V9(02) COMP-3.
120308       10 PO-DRVB0779-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0779-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB0779-KLZA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
120308       10 PO-DRVB0779-KTY-ANSPR-STATUS   PIC S9(004)      BINARY.
210415       10 PO-DRVB0779-KNR-ANSPR-STATUS   PIC S9(004)      BINARY.
210415       10 PO-DRVB0779-KLZA-CENTRAL-KZ    PIC  X(01).
210415       10 PO-DRVB0779-KLZA-EH-NNE        PIC S9(09)V9(01) COMP-3.
210415       10 PO-DRVB0779-KLZA-JEQF          PIC S9(03)V9(02) COMP-3.
210415       10 PO-DRVB0779-KLZA-ERF-INDX      PIC  X(01).
210415       10 PO-DRVB0779-KLZA-AS-CLUB-KZ    PIC  X(01).
210415       10 PO-DRVB0779-KLZA-BES-345-KZ    PIC  X(01).
210415       10 PO-DRVB0779-KLZA-STRUKTUR      PIC S9(04)       BINARY.
210415       10 PO-DRVB0779-KLZA-PRAXIS        PIC S9(04)       BINARY.
210415       10 PO-DRVB0779-KLZA-AUSTRITT-KZ   PIC  X(01).
210415       10 PO-DRVB0779-KLZA-BES-229-KZ    PIC  X(01).
210415       10 PO-DRVB0779-KLZA-BV-FEHLT-KZ   PIC  X(01).
210415       10 PO-DRVB0779-KLZA-PFAEND-KZ     PIC  X(01).
210415       10 PO-DRVB0779-KLZA-SPERRE-KZ     PIC  X(01).
210415       10 PO-DRVB0779-KLZA-SPERRE-TEXT   PIC  X(40).
      *
      ***** ENDE COPY-MEMBER DRVB0779 *****************************
      *

