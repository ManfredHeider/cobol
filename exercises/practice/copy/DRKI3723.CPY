      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3723.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3723.CPv  $
      *
      *   Rev 2.2   Sep 20 2019 10:54:48   t13ac23
      *AKI01717: Neue KD-Zuordnung 
      *
      *   Rev 2.1   Sep 19 2019 08:40:34   t13ac23
      *AKI01717: Neue KD-Zuordnung
      *
      *   Rev 2.0   Sep 09 2019 10:44:42   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3723                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das Join-OZP ZJKI0039                    *
      *    VERTRAGROLLEBEZ zu KRANKENPERSEIGEN
      *    --------------------------------------------------------   *
      *    ERSTELLT: DETMAR                                           *
      *    DATUM:    09.09.2019                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01717!09.09.19!2.1 ! NEUERSTELLUNG             ! Detmar   *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3723                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRKI3723.
          05 PI-DRKI3723-PROGRAMM              PIC X(008).
          05 PI-DRKI3723-UFKT-CODE             PIC 9(002).
          05  PI-DRKI3723-DATEN.
              10  PI-DRKI3723-KTY              PIC  S9(04) COMP.
      *
       01  PO-DRKI3723.
           05  PO-DRKI3723-RC                  PIC  X(003).
               88 PO-DRKI3723-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3723-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI3723-RC-EOF                       VALUE 'EOF'.

           05  PO-DRKI3723-PERSONID-CNT        PIC  9(02) COMP.
           05  PO-DRKI3723-DATEN.
               10 PO-DRKI3723-KNR            PIC  S9(015)v USAGE COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


