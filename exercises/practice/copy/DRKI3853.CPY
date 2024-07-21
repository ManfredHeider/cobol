      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3853.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3853.CPv  $
      *
      *   Rev 2.3   Jan 11 2021 16:25:14   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.2   Jan 11 2021 16:10:04   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.1   Jan 06 2021 10:18:26   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.0   Dec 22 2020 09:00:26   u130685
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3853                                      *
      *    --------------------------------------------------------   *
      *    CURSOR FULL SELECT ZXKI0536 G33_SPARPLAN                   *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01821!22.12.20!1.0 !NEU FUER DVAG WIEN SPARPLAN! U130685  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3853
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3853.
           05 PI-DRKI3853-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3853-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3853-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3853-UFKT-QUIT               VALUE 3.
 
 
       01  PO-DRKI3853.
           05 PO-DRKI3853-KEY.
              10 PO-DRKI3853-G33VNR            PIC X(15).
              10 PO-DRKI3853-G33LFDNR          PIC S9(4) COMP.
 
           05 PO-DRKI3853-RC                    PIC  X(0003).
              88 PO-DRKI3853-RC-OK              VALUE 'OK '.
              88 PO-DRKI3853-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3853-RC-EOF             VALUE 'EOF'.
*
           05  PO-DRKI3853-DATEN.
              10 PO-DRKI3853-G33ABRKTO   PIC  X(0020).
              10 PO-DRKI3853-G33ABWEICHKTOINH   PIC  X(0030).
              10 PO-DRKI3853-G33AUSGABEAKTFOND   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3853-G33AUSGABEURFOND   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3853-G33BEGINNDYN   PIC  X(0008).
              10 PO-DRKI3853-G33BEGINNRATENAUS   PIC  X(0008).
              10 PO-DRKI3853-G33BIC   PIC  X(0011).
              10 PO-DRKI3853-G33BONI   PIC S9(3)V9(4) COMP-3.
              10 PO-DRKI3853-G33DATVERTRAGKUEND   PIC S9(9) COMP.
              10 PO-DRKI3853-G33ENDERATENAUS   PIC  X(0008).
              10 PO-DRKI3853-G33IBAN   PIC  X(0034).
              10 PO-DRKI3853-G33ISIN   PIC  X(0012).
              10 PO-DRKI3853-G33KOSTENPROZENT   PIC S9(1)V9(2) COMP-3.
              10 PO-DRKI3853-G33KUENDMERKMAL   PIC  X(0001).
              10 PO-DRKI3853-G33MANDAT   PIC  X(0035).
              10 PO-DRKI3853-G33PIN   PIC S9(9) COMP.
              10 PO-DRKI3853-G33PROZENTDYNAMIS   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3853-G33SPARPLANANF   PIC  X(0008).
              10 PO-DRKI3853-G33SPARPLANENDE   PIC  X(0008).
              10 PO-DRKI3853-G33SPARPLANENDE2   PIC  X(0008).
              10 PO-DRKI3853-G33SPARRATE   PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3853-G33TAGEINZAHLUNG   PIC S9(4) COMP.
              10 PO-DRKI3853-G33URGESAMTEURO   PIC S9(6)V9(2) COMP-3.
              10 PO-DRKI3853-G33URSPARRATE   PIC S9(4)V9(2) COMP-3.
              10 PO-DRKI3853-G33VERTRAGLFZ   PIC S9(9) COMP.
              10 PO-DRKI3853-G33VERTRAGSTYP   PIC  X(0002).
              10 PO-DRKI3853-G33WKN   PIC  X(0006).
              10 PO-DRKI3853-G33ZAHLWEISE   PIC S9(9) COMP.
              10 PO-DRKI3853-G33ZLASTRUECK   PIC S9(9) COMP.
      ******* ENDE COPY-MEMBER ***************************************



