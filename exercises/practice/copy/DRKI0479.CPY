      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0479.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0479.CPv  $
      *
      *   Rev 2.5   Jan 18 2006 16:12:20   t13ex39
      *AKI00513: Bestandsdatenerweiterung in der KI
      *
      *   Rev 2.4   Nov 18 2005 13:22:54   t13ex39
      *AKI00513: Bestandsdatenerweiterung in der KI
      *
      *   Rev 1.0   28 Jul 1998 09:57:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0479                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0479                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.11.2005/13:17:06
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
      *    FÜR DIE FUNKTION DRKI0479
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0479.
           05  PI-DRKI0479-KEY.


             10 PI-DRKI0479-VEGGEFAHRCODE   PIC S9(4) COMP.
             10 PI-DRKI0479-VEGVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0479-VEGVERTRAGSID   PIC X(13).
             

           05  PI-DRKI0479-DATEN.


             10 PI-DRKI0479-VEGKZSBNL   PIC X(1).
             10 PI-DRKI0479-VEGSBMAX   PIC S9(5)V COMP-3.
             10 PI-DRKI0479-VEGSBMIN   PIC S9(5)V COMP-3.
             10 PI-DRKI0479-VEGSBSCHADEN   PIC S9(2)V COMP-3.
             10 PI-DRKI0479-VEGSBVSUMME   PIC S9(2)V COMP-3.
             10 PI-DRKI0479-VEGSELBSTBETEIL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0479-NI-VEGSELBSTBETEIL   PIC S9(4) COMP.
             10 PI-DRKI0479-VEGSELBSTBETWKZ   PIC X(01).
             10 PI-DRKI0479-VEGTARIFZONE   PIC S9(4) COMP.
             
      *
       01  PO-DRKI0479.
           05  PO-DRKI0479-RC                 PIC  X(003).
               88 PO-DRKI0479-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0479-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
