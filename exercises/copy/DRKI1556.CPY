      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1556.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1556.CPv  $
      *
      *   Rev 2.3   14 May 2007 15:50:36   t13ex43
      *AKI00610: Außendienst KI Depotausdruck
      *
      *   Rev 2.0   May 19 2003 09:59:00   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1556                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1556                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  08.05.2007/12:53:20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1556
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1556.
           05  PI-DRKI1556-KEY.


             10 PI-DRKI1556-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1556-GHPVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1556-DATEN.


             10 PI-DRKI1556-GHPPERFDAT   PIC X(10).
             10 PI-DRKI1556-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI1556-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             

      *
       01  PO-DRKI1556.
           05  PO-DRKI1556-RC                 PIC  X(003).
               88 PO-DRKI1556-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1556-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
