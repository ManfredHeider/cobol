      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2191.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2191.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 13:17:46   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:36:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2191                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2191                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/12:08:14
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
      *    FÜR DIE FUNKTION DRKI2191
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2191.
           05  PI-DRKI2191-KEY.


             10 PI-DRKI2191-VRMRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2191-VRMRISIMERKMALID   PIC S9(4) COMP.
             10 PI-DRKI2191-VRMVERTRAGSID   PIC X(13).
             10 PI-DRKI2191-VRMVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2191-DATEN.


             10 PI-DRKI2191-VRMBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2191-VRMEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2191-VRMRISMERKMWERT   PIC X(50).
             

      *
       01  PO-DRKI2191.
           05  PO-DRKI2191-RC                 PIC  X(003).
               88 PO-DRKI2191-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2191-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
