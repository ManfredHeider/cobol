      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2190.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2190.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 13:17:34   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:33:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2190                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2190                    *
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
      *    FÜR DIE FUNKTION DRKI2190
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2190.
           05  PI-DRKI2190-KEY.


             10 PI-DRKI2190-VRMRISIGRUPPEID   PIC S9(4) COMP.
             10 PI-DRKI2190-VRMRISIMERKMALID   PIC S9(4) COMP.
             10 PI-DRKI2190-VRMVERTRAGSID   PIC X(13).
             10 PI-DRKI2190-VRMVETLFDNR   PIC S9(4) COMP.
             

           05  PI-DRKI2190-DATEN.


             10 PI-DRKI2190-VRMBEZEICHNUNG   PIC X(50).
             10 PI-DRKI2190-VRMEINZVTGHOSTID   PIC X(15).
             10 PI-DRKI2190-VRMRISMERKMWERT   PIC X(50).
             
      *
       01  PO-DRKI2190.
           05  PO-DRKI2190-RC                 PIC  X(003).
               88 PO-DRKI2190-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2190-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
