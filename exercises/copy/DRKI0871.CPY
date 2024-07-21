      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0871.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0871.cpv  $
      *
      *   Rev 2.1   Jun 24 1999 15:42:08   I13PC18
      *aki00033
      *
      *   Rev 2.0   Apr 29 1999 13:57:04   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *  Insert Satz
      *****************************************************************
      *    COPY-MEMBER: DRKI0871                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0873                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   manuell erzeugt
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00027!22.6.99 ! 1.0! NEU                       ! MHA      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0871
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0871.
           05  PI-DRKI0871-KEY.


             10 PI-DRKI0871-IBTID   PIC X(13).
             10 PI-DRKI0871-IBTVBNR PIC S9(9) COMP.

           05  PI-DRKI0871-DATEN.

           10 PI-DRKI0871-IBTPCVON   PIC S9(9) COMP.
           10 PI-DRKI0871-IBTPCNACH  PIC S9(9) COMP.

      *
       01  PO-DRKI0871.
           05  PO-DRKI0871-RC                 PIC  X(003).
               88 PO-DRKI0871-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0871-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
