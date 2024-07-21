      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0839.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki0839.cpv  $
      *
      *   Rev 2.2   Jul 07 1999 12:08:32   T13EX19
      *fki00077 - DRKI0833 und DRKI0894 neu
      *
      *   Rev 2.1   Apr 29 1999 10:37:44   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:58:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:51:22   P13SP20
      *Initial revision.
      *---------------------------------------------------------------*
      *    COPY-MEMBER: drki0839                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO zjki0016                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    24.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00029!24.04.99! 01 ! NEU                       ! ZGUR-VUJIC
      * FKI00077!01.07.99!    ! neue Fkt DRKI0883,DRKI0894!          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-zjki0016.
           05  PI-zjki0016-FUNKTION            PIC  X(008).
               88 PI-zjki0016-drki0840              VALUE 'drki0840'.
               88 PI-zjki0016-drki0883              VALUE 'drki0883'.
               88 PI-zjki0016-drki0894              VALUE 'drki0894'.
      *
      ******* ENDE COPY-MEMBER drki0839 *****************************
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
