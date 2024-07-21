      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  10.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3465.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3465.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:51:46   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.2   Oct 27 2018 15:29:36   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.1   Sep 21 2018 10:14:52   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.0   Sep 10 2018 15:47:42   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3465                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0516 G42_BANKPRODUKT                *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!13.09.18!2.1 ! Neuerstellung             ! tfl      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3465
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3465.
           05 PI-DRKI3465-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3465-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3465-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3465-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3465-KEY.
               10 PI-DRKI3465-G42PMANDANT     PIC S9(009) COMP.
 
      *
       01  PO-DRKI3465.
           05 PO-DRKI3465-RC                   PIC  X(003).
              88 PO-DRKI3465-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3465-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3465-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3465-DATEN.
              10 PO-DRKI3465-G42PVERTRAGSNR   PIC  X(0015).
              10 PO-DRKI3465-G42PAKTMONAT   PIC  X(0005).
              10 PO-DRKI3465-G42PBUCHDAT   PIC  X(0008).
              10 PO-DRKI3465-G42PDATEROFFNUNG   PIC  X(0008).
              10 PO-DRKI3465-G42PDATSCHLIESS   PIC  X(0008).
              10 PO-DRKI3465-G42PEXTLIMIT   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3465-G42PHABENZINS   PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3465-G42PKONTOART   PIC  X(0002).
              10 PO-DRKI3465-G42PKUMHABENVM   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3465-G42PKUMSOLLVM   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3465-G42PSALDOBETR   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3465-G42PSOLLZINS   PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3465-G42PSUMHABENAM   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3465-G42PSUMSOLLAM   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3465-G42PUEBERZZINS   PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3465-G42PVORMONAT   PIC  X(0005).
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 

