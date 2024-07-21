      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0219.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0219.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:53:28   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0219                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0219                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Gsellhofer                                       *
      *    DATUM:    28.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY  1!28.01.97! 01 ! NEU                       ! Gsellh.  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0219
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0219 DES FÜHRT Den Update der Antragsdatei
      *    aus
      *
      ****************************************************************
      *
       01  PI-DRPA0219.
           05  PI-DRPA0219-UFKT                PIC  X(01).
               88 PI-DRPA0219-UAUN            value '1'.
               88 PI-DRPA0219-U1U2            value '2'.
               88 PI-DRPA0219-D1I2            value '3'.

           05  PI-DRPA0219-ANKEY-ALT.
               10 PI-DRPA0219-AMANR            PIC  9(07) COMP-3.
               10 PI-DRPA0219-AGES             PIC  9(02).
               10 PI-DRPA0219-AVNR             PIC  9(15) COMP-3.

           05  PI-DRPA0219-TDQ-HEADER.
               10 PI-DRPA0219-ABMODUS          PIC  X(02).
               10 PI-DRPA0219-ABDATUM          PIC  9(07) COMP-3.
               10 PI-DRPA0219-ABUHR            PIC  9(09) COMP-3.
               10 PI-DRPA0219-ABTERM           PIC  X(04).
               10 PI-DRPA0219-ABUSERID         PIC  X(07).

      ***  ENTSPRICHT DEM COPY 'UU020S'
           05  PI-DRPA0219-ANSATZ-NEU.
               10  ASA                     PIC 99.
               10  ANKEY.
                   20  AMANR               PIC 9(7)        COMP-3.
                   20  AGES                PIC 99.
                   20  AVNR                PIC 9(15)       COMP-3.
      *   *
      *   *    BESTANDS-SATZ
      *   *
               10  ASATZ.
                   20  APRZMA              PIC X.
                   20  APRZVG              PIC XX.
                   20  AMATCHCODE          PIC X(8).
                   20  FILLER  REDEFINES AMATCHCODE.
                       30  ACODE4          PIC X(4).
                       30  FILLER          PIC X(4).
                   20  FILLER  REDEFINES AMATCHCODE.
                       30  ACODE5          PIC X(5).
                       30  FILLER          PIC X(3).
                   20  FILLER  REDEFINES AMATCHCODE.
                       30  ACODE6          PIC X(6).
                       30  FILLER          PIC X(2).
                   20  FILLER  REDEFINES AMATCHCODE.
                       30  ACODE7          PIC X(7).
                       30  FILLER          PIC X(1).
                   20  AMANRVM             PIC 9(7)        COMP-3.
                   20  APRZVM              PIC X.
                   20  APRODS.
                       30  APRODSCHL       PIC XX.
                       30  APRODSCHL2      PIC XX.
                       30  APRODSCHL3      PIC XX.
                       30  APRODSCHL4      PIC XX.
                       30  APRODSCHL5      PIC XX.
                       30  APRODSCHL6      PIC XX.
                       30  APRODSCHL7      PIC XX.
                       30  APRODSCHL8      PIC XX.
                       30  APRODSCHL9      PIC XX.
                   20  APROD     REDEFINES     APRODS.
                       30  APRS            PIC XX          OCCURS 9.
                   20  ATARIF              PIC X(10).
                   20  AANREDE             PIC X.
                   20  AVNAME              PIC X(26).
                   20  AZUNAME             PIC X(26).
                   20  ASTR                PIC X(26).
                   20  AAUSL               PIC X(3).
                   20  APLZ                PIC X(9).
                   20  AORT                PIC X(21).
                   20  AEALTER             PIC 99.
                   20  ADAUER              PIC 99.
                   20  ABEGINN             PIC 9(7)        COMP-3.
                   20  AABLAUF             PIC 9(7)        COMP-3.
                   20  AZAHLW              PIC 9.
                   20  ABUZPROZ            PIC 99.
                   20  AEINH.
                       30  AEREINH         PIC S9(6)V9     COMP-3.
                       30  AEREINH2        PIC S9(6)V9     COMP-3.
                       30  AEREINH3        PIC S9(6)V9     COMP-3.
                       30  AEREINH4        PIC S9(6)V9     COMP-3.
                       30  AEREINH5        PIC S9(6)V9     COMP-3.
                       30  AEREINH6        PIC S9(6)V9     COMP-3.
                       30  AEREINH7        PIC S9(6)V9     COMP-3.
                       30  AEREINH8        PIC S9(6)V9     COMP-3.
                       30  AEREINH9        PIC S9(6)V9     COMP-3.
                   20  ABEINH      REDEFINES   AEINH.
                       30  AABEIN          PIC S9(6)V9     COMP-3
                                                         OCCURS 9.
                   20  AANSUMMEN.
                       30  AANSUMME        PIC S9(7)       COMP-3.
                       30  AANSUMME2       PIC S9(7)       COMP-3.
                       30  AANSUMME3       PIC S9(7)       COMP-3.
                       30  AANSUMME4       PIC S9(7)       COMP-3.
                       30  AANSUMME5       PIC S9(7)       COMP-3.
                       30  AANSUMME6       PIC S9(7)       COMP-3.
                       30  AANSUMME7       PIC S9(7)       COMP-3.
                       30  AANSUMME8       PIC S9(7)       COMP-3.
                       30  AANSUMME9       PIC S9(7)       COMP-3.
                   20  AANS      REDEFINES     AANSUMMEN.
                       30  AANSUM          PIC S9(7)       COMP-3
                                                         OCCURS 9.
                   20  AANBEITRAG          PIC S9(7)V99    COMP-3.
                   20  AAPEINH             PIC S9(6)V9     COMP-3.
                   20  AAPSUMME            PIC S9(7)       COMP-3.
                   20  AAPBEITRAG          PIC S9(5)V99    COMP-3.
                   20  ASCHNR              PIC 9(13)       COMP-3.
                   20  ASCHBET             PIC S9(5)V99    COMP-3.
                   20  ASCHECK-FELDER-NEU REDEFINES ASCHBET.
                       30  ASCHECK             PIC X.
                       30  ASCHFILLER          PIC X(3).
                   20  ABLTZ               PIC 9(9)        COMP-3.
                   20  AKTONR              PIC 9(11)       COMP-3.
                   20  AANDATUM            PIC S9(7)       COMP-3.
                   20  AANERFDAT           PIC 9(7)        COMP-3.
                   20  AANSTATUS1          PIC 99.
                   20  AANSTATUS2          PIC 99.
                   20  AANSTATUS3          PIC 99.
                   20  ASTATDAT1           PIC 9(7)        COMP-3.
                   20  ASTATDAT2           PIC 9(7)        COMP-3.
                   20  ASTATDAT3           PIC 9(7)        COMP-3.
                   20  AANART              PIC XX.
                   20  ADIREKTION          PIC 9(3).
                   20  AVKBZ               PIC 9.
                   20  ADTADAT             PIC 9(7)        COMP-3.
                   20  AAG8-AEND           PIC X.
                   20  ABETREUER   OCCURS 18.
                       30  AGMANR          PIC 9(7)        COMP-3.
                       30  AGPRZ           PIC X.
                       30  AGSTRSTUFE      PIC 9(3)        COMP-3.
                       30  AGSTRTEXT       PIC X(4).
                       30  AGPROZ          PIC 9(4)V9      COMP-3.
                   20  AIVSNR              PIC 9(7)        COMP-3.
                   20  AKRED               PIC 9(7)V99     COMP-3.
                   20  APROV               PIC 9(5)V99     COMP-3.
                   20  AABR-KZ             OCCURS 6.
                       30  AAB-KZ          PIC X.
                   20  AABFAELLIG1         PIC S9(7)       COMP-3.
                   20  AEA40X              PIC X.
                   20  ABUZDAUER           PIC 99.
                   20  ABAVKZ              PIC X.
                   20  AAFOLGE             PIC X.
                   20  AABFAELLIG2         PIC S9(7)       COMP-3.
                   20  AABFAELLIG3         PIC S9(7)       COMP-3.
                   20  AABFAELLIG4         PIC S9(7)       COMP-3.
                   20  AABFAELLIG5         PIC S9(7)       COMP-3.
                   20  AABFAELLIG6         PIC S9(7)       COMP-3.
                   20  AHISKZ              PIC X.
                   20  AVZAN               PIC X.
                   20  AGEBDAT             PIC S9(7)       COMP-3.
                   20  AVERSDAUER-HV       PIC 9(02).
                   20  AEINALTER1          PIC 9(02).
                   20  AEINALTER2          PIC 9(02).
                   20  FILLER              PIC X(13).
      *
       01  PO-DRPA0219.
           05  PO-DRPA0219-RC                  PIC  X(03).
               88 PO-DRPA0219-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0219-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
