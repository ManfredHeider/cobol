      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  17.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3763.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3763.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 13:12:06   t13ex78
      *AKI01731: List OZP G66_PRIVATKREDIT
      *
      *   Rev 2.0   Jan 17 2020 11:17:54   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3763                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0538 G66_PRIVATKREDIT               *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01731!14.01.20!2.0 ! Neuerstellung             ! t13ex78   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3763
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
      *
       01  PI-DRKI3763.
           05 PI-DRKI3763-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3763-UFKT-FIRST        VALUE 1.
              88 PI-DRKI3763-UFKT-NEXT         VALUE 2.
              88 PI-DRKI3763-UFKT-QUIT         VALUE 3.
 
           05  PI-DRKI3763-KEY.
               10 PI-DRKI3763-G66PMANDANT      PIC S9(9) COMP.
 
      *
       01  PO-DRKI3763.
           05 PO-DRKI3763-RC                   PIC  X(003).
              88 PO-DRKI3763-RC-OK             VALUE 'OK '.
              88 PO-DRKI3763-RC-LEER           VALUE 'LER'.
              88 PO-DRKI3763-RC-EOF            VALUE 'EOF'.
      *
           05 PO-DRKI3763-DATEN.
              10 PO-DRKI3763-G66PVERTRAGSNR    PIC X(15).
              10 PO-DRKI3763-G66PZUSTAND       PIC X(01).
              10 PO-DRKI3763-G66PBEGINNDATUM   PIC X(08).
              10 PO-DRKI3763-G66PENDEDATUM     PIC X(08).
              10 PO-DRKI3763-G66PABSCHLVBNR    PIC S9(9) COMP.
              10 PO-DRKI3763-G66PDARLBETR      PIC S9(12)V99 COMP-3.
              10 PO-DRKI3763-G66PKREDITZUSDAT  PIC X(08).
              10 PO-DRKI3763-G66PKREDITAUSDAT  PIC X(08).
              10 PO-DRKI3763-G66PKREDITART     PIC X(06).
              10 PO-DRKI3763-G66PSALDOBETR     PIC S9(12)V99 COMP-3.
              10 PO-DRKI3763-G66PZINSSATZ      PIC S9(02)V9(04) COMP-3.
              10 PO-DRKI3763-G66PRATEBETR      PIC S9(12)V99 COMP-3.
              10 PO-DRKI3763-G66PFAELLIGDAT    PIC X(08).
              10 PO-DRKI3763-G66PSICHERHEIT    PIC X(01).
              10 PO-DRKI3763-G66PRESTLAUFZEIT  PIC X(03).
              10 PO-DRKI3763-G66PIBANBELAST    PIC X(34).
              10 PO-DRKI3763-G66PBICBELAST     PIC X(11).
              10 PO-DRKI3763-G66PMANDATSREF    PIC X(35).
              10 PO-DRKI3763-G66PSEPANACHNAME  PIC X(30).
              10 PO-DRKI3763-G66PSEPAVORNAME   PIC X(30).
              10 PO-DRKI3763-G66PIBANVERTRAG   PIC X(34).
 
      ******* ENDE COPY-MEMBER ***************************************

