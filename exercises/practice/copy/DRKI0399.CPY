      * T13EX33   Stand: 13/08/98    15:53:26
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0399.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0399.CPv  $
      *
      *   Rev 2.0   Mar 23 1999 15:52:58   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:04:54   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Delete LTS AML:tgl. Akt.

      *****************************************************************
      *    COPY-MEMBER: DRKI0399                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0399                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  13.08.98/14.20.26
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
      *    FÜR DIE FUNKTION DRKI0399
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0399.
           05  PI-DRKI0399-KEY.


             10 PI-DRKI0399-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI0399-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0399-LTSVERTRAGSID   PIC X(13).
             10 PI-DRKI0399-LTSZAHLDYNERHOEH   PIC S9(4) COMP.

      *
       01  PO-DRKI0399.
           05  PO-DRKI0399-RC                 PIC  X(003).
               88 PO-DRKI0399-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0399-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
