      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0247.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0247.cpv  $
      *
      *   Rev 2.1   Oct 29 1999 17:40:40   T13AE42
      *AOP00177 - neue Funktion 80 SL
      *eine Eingabe
      *
      *   Rev 2.0   Feb 27 1999 15:58:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:21:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0247                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0247                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.SCHMIDT                                       *
      *    DATUM:    29.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!29.10.99! 01 ! NEU                       ! B.SCHMIDT*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0247
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    FUNKTION:
      *            UMRECHNEN VON EINER 6STELLIGEN JAHRSZAHL DES
      *            FORMATS JJMMTT IN EINE 8STELLIGE JAHRESZAHL DES
      *            FORMATES JJJJMMTT MIT SL GRENZE 80
      *                                  -- ---------
      *            1 PI
      *
      ****************************************************************
       01  PI-DRVB0247.
           05  PI-DRVB0247-JAHR-1             PIC  9(006).
      *
       01  PO-DRVB0247.
           05  PO-DRVB0247-RC                 PIC  X(003).
               88 PO-DRVB0247-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0247-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0247-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0247-JAHR-1             PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************

