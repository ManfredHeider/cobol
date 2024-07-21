      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2187.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2187.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:52:52   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.0   29 Sep 2007 12:24:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2187                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2187                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.05.2008/16:31:30
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
      *    FÜR DIE FUNKTION DRKI2187
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2187.
           05  PI-DRKI2187-KEY.


             10 PI-DRKI2187-VRIRISIKOGRUPPID   PIC S9(4) COMP.
             10 PI-DRKI2187-VRIRISIKOLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2187-VRITHEMGEBSL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2187-VRIVERTRAGSID   PIC X(13).
             10 PI-DRKI2187-VRIVETLFDNR   PIC S9(4) COMP.

      *
       01  PO-DRKI2187.
           05  PO-DRKI2187-RC                 PIC  X(003).
               88 PO-DRKI2187-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2187-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
