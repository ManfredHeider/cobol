      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3446.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3446.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 10:15:58   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.2   Oct 27 2018 15:24:24   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.1   Oct 25 2018 14:44:28   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.0   Sep 07 2018 11:23:24   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3446                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0515 G42_DEPOTSPARPLAN              *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!13.09.18!2.1 ! Neuerstellung             ! tfl      *
      * --------!--------!----!---------------------------!----------*
      * DBO-8960!30.08.21!    !Neues Attribut SSTATUS     ! ATAR     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3446
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3446.
           05 PI-DRKI3446-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3446-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3446-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3446-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3446-KEY.
               10 PI-DRKI3446-G42SMANDANT     PIC  S9(009) COMP.
 
      *
       01  PO-DRKI3446.
           05 PO-DRKI3446-RC                   PIC  X(003).
              88 PO-DRKI3446-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3446-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3446-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3446-DATEN.
              10 PO-DRKI3446-KEY.
                 15 PO-DRKI3446-G42SVERTRAGSNR  PIC  X(0015).
                 15 PO-DRKI3446-G42SLFDNR   PIC S9(4) COMP.
              10 PO-DRKI3446-G42SAAUFSCHLAKT   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3446-G42SAAUFSCHLURSPR   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3446-G42SBEGDATDYNAM   PIC  X(0008).
              10 PO-DRKI3446-G42SBIC   PIC  X(0011).
              10 PO-DRKI3446-G42SBONIFIKATION   PIC S9(3)V9(4) COMP-3.
              10 PO-DRKI3446-G42SDATBEGINNAUS   PIC  X(0008).
              10 PO-DRKI3446-G42SDATENDEAUS   PIC  X(0008).
              10 PO-DRKI3446-G42SDATVERAUFLOE   PIC  X(0008).
              10 PO-DRKI3446-G42SDYNAMIK PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3446-G42SEINAUSDATUM   PIC  X(0002).
              10 PO-DRKI3446-G42SEINAUSTYP   PIC  X(0002).
              10 PO-DRKI3446-G42SEINZAHFRISTEND   PIC  X(0008).
              10 PO-DRKI3446-G42SENDEDAT   PIC  X(0008).
              10 PO-DRKI3446-G42SIBAN   PIC  X(0034).
              10 PO-DRKI3446-G42SISIN   PIC  X(0012).
              10 PO-DRKI3446-G42SKONTOINHABER   PIC  X(0030).
              10 PO-DRKI3446-G42SKOSTENPROZ   PIC S9(1)V9(2) COMP-3.
              10 PO-DRKI3446-G42SKUENDMERKMAL   PIC  X(0001).
              10 PO-DRKI3446-G42SMANDREF       PIC  X(0035).
              10 PO-DRKI3446-G42SOEFDAT   PIC  X(0008).
              10 PO-DRKI3446-G42SSPARRATE  PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3446-G42SURSPARRATE   PIC S9(4)V9(2) COMP-3.
              10 PO-DRKI3446-G42SURSPRSPARLST   PIC S9(6)V9(2) COMP-3.
              10 PO-DRKI3446-G42SVERTLAUFZEIT   PIC S9(4) COMP.
              10 PO-DRKI3446-G42SWKN   PIC  X(0006).
              10 PO-DRKI3446-G42SZAHLWEISE   PIC  X(0001).
              10 PO-DRKI3446-G42SZLASTRUECK   PIC S9(4) COMP.
              10 PO-DRKI3446-G42SSTATUS   PIC X(0001).
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 

