      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0566.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0566.CPv  $
      *
      *   Rev 2.2   Apr 29 2009 14:58:04   t13ex14
      *AKI00818: Tabelle VERTRAGRABATT OZP-Objekte
      *
      *   Rev 1.0   07 Aug 1998 14:48:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0566                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0566                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  22.04.2009/15:54:27
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
      *    FÜR DIE FUNKTION DRKI0566
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0566.
           05  PI-DRKI0566-KEY.


             10 PI-DRKI0566-VRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI0566-VRAVERTRAGSID   PIC X(13).
             

           05  PI-DRKI0566-DATEN.


             10 PI-DRKI0566-VRARABEINHEIT   PIC S9(4) COMP.
             10 PI-DRKI0566-VRARABTEXT   PIC X(26).
             10 PI-DRKI0566-VRARABWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0566-VRARABWERWKZ   PIC X(01).
             
      *
       01  PO-DRKI0566.
           05  PO-DRKI0566-RC                 PIC  X(003).
               88 PO-DRKI0566-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0566-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
