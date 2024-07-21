      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2193.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2193.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 13:18:10   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:41:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2193                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2193                    *
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
      *    FÜR DIE FUNKTION DRKI2193
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2193.
           05  PI-DRKI2193-KEY.


             10 PI-DRKI2193-VRMRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2193-VRMRISIMERKMALID   PIC S9(4) COMP.
             10 PI-DRKI2193-VRMVERTRAGSID   PIC X(13).
             10 PI-DRKI2193-VRMVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2193.
           05  PO-DRKI2193-RC                 PIC  X(003).
               88 PO-DRKI2193-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2193-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2193-DATEN.


             10 PO-DRKI2193-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2193-VRMBEZEICHNUNG   PIC X(50).
             10 PO-DRKI2193-VRMEINZVTGHOSTID   PIC X(15).
             10 PO-DRKI2193-VRMRISMERKMWERT   PIC X(50).
             

      *
      ******* ENDE COPY-MEMBER **************************************
