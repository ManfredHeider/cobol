      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0473.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI0473.CPv  $
      *
      *   Rev 2.7   Jan 18 2006 16:12:12   t13ex39
      *AKI00513: Bestandsdatenerweiterung in der KI
      *
      *   Rev 2.6   Jan 18 2006 09:01:54   t13ex39
      *AKI00513: Bestandsdatenerweiterung in der KI
      *
      *   Rev 1.0   28 Jul 1998 09:57:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0473                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0473                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.01.2006/08:26:31
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
      *    FÜR DIE FUNKTION DRKI0473
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0473.
           05  PI-DRKI0473-KEY.


             10 PI-DRKI0473-VUMUMFANGID   PIC S9(9) COMP.
             10 PI-DRKI0473-VUMUMFANGLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0473-VUMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0473-VUMVERTRAGSID   PIC X(13).
             

           05  PI-DRKI0473-DATEN.


             10 PI-DRKI0473-VUMEGREH   PIC X(1).
             10 PI-DRKI0473-VUMUMFANGPROZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0473-VUMUMFANGTEXT.
                15 PI-DRKI0473-VUMUMFANGTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI0473-VUMUMFANGTEXT-TEXT   PIC X(20).
                
             10 PI-DRKI0473-VUMUMFEGR   PIC S9(9) COMP.
             
      *
       01  PO-DRKI0473.
           05  PO-DRKI0473-RC                 PIC  X(003).
               88 PO-DRKI0473-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0473-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
