      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2197.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2197.CPv  $
      *
      *   Rev 2.2   Jun 21 2008 12:53:36   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.1   17 Oct 2007 13:18:58   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   29 Sep 2007 12:53:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2197                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2197                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.10.2007/12:16:26
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
      *    FÜR DIE FUNKTION DRKI2197
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2197.
           05  PI-DRKI2197-KEY.


             10 PI-DRKI2197-VZAUNIVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2197-VZAVERTRAGSID   PIC X(13).
             10 PI-DRKI2197-VZAVETLFDNR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2197.
           05  PO-DRKI2197-RC                 PIC  X(003).
               88 PO-DRKI2197-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2197-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
